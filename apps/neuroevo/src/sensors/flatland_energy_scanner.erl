-module(flatland_energy_scanner).
-export([sense/4]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").
-include("flatland.hrl").

sense(Exoself_PId, VL, [Spread, Density, RadialOffset], Scape) ->
    case gen_server:call(Scape, {get_all, avatars, Exoself_PId}) of
        destroyed ->
            lists:duplicate(VL, -1);
        Avatars ->
            Self = lists:keyfind(Exoself_PId, 2, Avatars),
            Loc = Self#avatar.loc,
            Direction = Self#avatar.direction,
            Result = energy_scanner(silent, {1,0,0}, Density, Spread, Loc, Direction, lists:keydelete(Exoself_PId, 2, Avatars)),
            Result
    end.

% Input: ViewAngle=Radian, Density=n, Gaze direction={SensorLoc,Direction}.
% Output: List of ranges 1/Distance no intersection = -1, with angle starting with (Gaze + ViewAngle/2), and ending with (Gaze - ViewAngle/2), [Dist1...DistDensity].
energy_scanner(Op, {Zoom, PanX, PanY}, Density, Spread, Loc, Direction, Avatars) ->
    case util:is_even(Density) of
        true ->
            Resolution = Spread / Density,
            SAngle = (Density / 2) * Resolution,
            StartAngle = -SAngle + Resolution / 2;
        false ->
            Resolution = Spread / Density,
            SAngle = trunc(Density / 2) * Resolution,
            StartAngle = -SAngle
    end,
    UnitRays = lib_sensor_flatland:create_UnitRays(Direction, Density, Resolution, StartAngle, []),
    EnergyScanList = compose_EnergyScanList(Loc, UnitRays, Avatars, []),
    %?DBG("EnergyScanList:~p~n", [EnergyScanList]),
    case Op of
        silent ->
            done;
        draw ->
            void
    end,
    EnergyScanList.

% 返回Energy列表
compose_EnergyScanList(Loc, [Ray|UnitRays], Avatars, Acc) ->
    {_Distance, _Color, Energy} = lib_sensor_flatland:shortest_intrLine2({Loc, Ray}, Avatars, {inf, void}, 0),
    compose_EnergyScanList(Loc, UnitRays, Avatars, [Energy/100|Acc]);
compose_EnergyScanList(_Loc, [], _Avatars, Acc) ->
    lists:reverse(Acc).

