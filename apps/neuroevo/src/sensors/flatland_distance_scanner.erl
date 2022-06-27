-module(flatland_distance_scanner).
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
            Result = distance_scanner(silent, {1,0,0}, Density, Spread, Loc, Direction, lists:keydelete(Exoself_PId, 2, Avatars)),
            Result
    end.

% Input: ViewAngle=Radian, Density=n, Gaze direction={SensorLoc,Direction}.
% Output: List of ranges 1/Distance no intersection = -1, with angle starting with (Gaze + ViewAngle/2), and ending with (Gaze - ViewAngle/2), [Dist1...DistDensity].
distance_scanner(Op, {Zoom, PanX, PanY}, Density, Spread, Loc, Direction, Avatars) ->
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
    RangeScanList = compose_RangeScanList(Loc, UnitRays, Avatars, []),
    %?DBG("RangeScanList:~p~n", [RangeScanList]),
    case {Op, get(canvas)} of
        {silent, _} ->
            done;
        {draw, undefined} ->
            Canvas = gen_server:call(get(scape), get_canvas),
            put(canvas, Canvas);
        {draw, Canvas} ->
            {X, Y} = Loc,
            FLoc = {X * Zoom + PanX, Y * Zoom + PanY},
            ScanListP = lists:zip(UnitRays, RangeScanList),
            Ids = [gs:create(line, Canvas, [{coords, [FLoc, {(X + Xr * Scale) * Zoom + PanX, (Y + Yr * Scale) * Zoom + PanY}]}]) ||
                     {{Xr, Yr}, Scale} <- ScanListP, Scale =/= -1],
            timer:sleep(2),
            [gs:destroy(Id) || Id <- Ids]
    end,
    RangeScanList.

% 返回Distance列表
compose_RangeScanList(Loc, [Ray|UnitRays], Avatars, Acc) ->
    {Distance, _Color} = lib_sensor_flatland:shortest_intrLine({Loc, Ray}, Avatars, {inf, void}),
    compose_RangeScanList(Loc, UnitRays, Avatars, [Distance|Acc]);
compose_RangeScanList(_Loc, [], _Avatars, Acc) ->
    lists:reverse(Acc).

