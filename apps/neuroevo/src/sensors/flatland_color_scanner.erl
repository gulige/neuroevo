-module(flatland_color_scanner).
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
            Result = color_scanner(silent, {1,0,0}, Density, Spread, Loc, Direction, lists:keydelete(Exoself_PId, 2, Avatars)),
            Result
    end.

% Input: ViewAngle=Radian, Density=n, Gaze direction={SensorLoc,Direction}.
% Output: List of ranges 1/Distance no intersection = -1, with angle starting with (Gaze + ViewAngle/2), and ending with (Gaze - ViewAngle/2), [Dist1...DistDensity].
color_scanner(Op, {Zoom, PanX, PanY}, Density, Spread, Loc, Direction, Avatars) ->
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
    ColorScanList = compose_ColorScanList(Loc, UnitRays, Avatars, []),
    ?DBG("ColorScanList:~p~n", [ColorScanList]),
    case {Op, get(canvas)} of
        {silent, _} ->
            done;
        {draw, undefined} ->
            Canvas = gen_server:call(get(scape), get_canvas),
            put(canvas, Canvas);
        {draw, Canvas} ->
            {X, Y} = Loc,
            FLoc = {X * Zoom + PanX, Y * Zoom + PanY},
            ScanListP = lists:zip(UnitRays, ColorScanList),
            Ids = [gs:create(line, Canvas, [{coords, [FLoc, {(X + Xr * 25) * Zoom + PanX, (Y + Yr * 25) * Zoom + PanY}]},
                                            {fg, lib_sensor_flatland:val2clr(Color)}]) ||
                     {{Xr, Yr}, Color} <- ScanListP],
            timer:sleep(2),
            [gs:destroy(Id) || Id <- Ids]
    end,
    ColorScanList.

% 返回Color列表
compose_ColorScanList(Loc, [Ray|UnitRays], Avatars, Acc) ->
    {_Distance, Color} = lib_sensor_flatland:shortest_intrLine({Loc, Ray}, Avatars, {inf, void}),
    compose_ColorScanList(Loc, UnitRays, Avatars, [Color|Acc]);
compose_ColorScanList(_Loc, [], _Avatars, Acc) ->
    lists:reverse(Acc).

