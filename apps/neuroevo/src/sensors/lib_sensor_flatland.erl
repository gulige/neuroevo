-module(lib_sensor_flatland).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").
-include("flatland.hrl").


%% 辅助创建以{X,Y}为中心方向的向周围扇形散射的射线簇
create_UnitRays(_, 0, _, _, Acc) ->
    Acc;
create_UnitRays({X, Y}, Density, Resolution, Angle, Acc) ->
    ?DBG("Angle:~p~n", [Angle * 180 / math:pi()]),
    % 将{X,Y}方向逆时针旋转Angle角度
    UnitRay = {X * math:cos(Angle) - Y * math:sin(Angle), X * math:sin(Angle) + Y * math:cos(Angle)},
    create_UnitRays({X, Y}, Density - 1, Resolution, Angle + Resolution, [UnitRay|Acc]).

%% 获取离周围Avatars最近的距离和颜色（颜色代表着物体种类）
shortest_intrLine(Gaze, [Avatar|Avatars], Val) ->
    shortest_intrLine(Gaze, Avatars, intr(Gaze, Avatar#avatar.objects, Val));
shortest_intrLine(_Gaze, [], {Distance, Color}) ->
    case Distance of
        inf -> %TODO, perhaps absence of color should be -1, not 1.
            {-1, 1};
        0.0 ->
            {-1, 1};
        _ ->
            {Distance, clr2val(Color)}
    end.

%% 获取离周围Avatars最近的距离、颜色（颜色代表着物体种类）和能量
shortest_intrLine2(Gaze, [Avatar|Avatars], Val, Energy) ->
    {D, _} = Val,
    {U_D, U_C} = intr(Gaze, Avatar#avatar.objects, Val),
    U_Energy = case D =:= U_D of
        true ->
            Energy;
        false -> % 不相等，则说明找到了距离更短的，用新Avatar的energy
            Avatar#avatar.energy
    end,
    shortest_intrLine2(Gaze, Avatars, {U_D, U_C}, U_Energy);
shortest_intrLine2(_Gaze, [], {Distance, Color}, Energy) ->
    case Distance of
        inf -> %TODO, perhaps absence of color should be -1, not 1.
            {-1, 1, Energy};
        0.0 ->
            {-1, 1, Energy};
        _ ->
            {Distance, clr2val(Color), Energy}
    end.

%% 寻找与avatar各部件的最近距离和颜色
intr(Gaze, [{circle, _Id, Color, _Pivot, C, R}|Objects], {Min, MinColor}) ->
    {S, D} = Gaze, % 自己的位置和朝向
    [{Xc, Yc}] = C, % 部件圆的圆心
    {Xs, Ys} = S, % 自己的位置
    {Xd, Yd} = D, % 自己的朝向，单位矢量
    {Xv, Yv} = {Xs - Xc, Ys - Yc}, % 部件到自己的矢量
    VdotD = Xv * Xd + Yv * Yd, % 两个矢量（部件到自己的矢量v、自己的朝向d）的点积
    Dis = Xv * Xv + Yv * Yv - VdotD * VdotD - R * R, % 垂直于自己朝向的距离平方
    ?DBG("S:~p D:~p C:~p V:~p R:~p VdotD:~p Dis:~p~n",[S, D, C, {Xv, Yv}, R, VdotD, Dis]),
    Result = case Dis > 0 of
        false ->
            inf;
        true ->
            SqrtDis = math:sqrt(Dis),
            I1 = -VdotD - SqrtDis,
            I2 = -VdotD + SqrtDis,
            case (I1 > 0) andalso (I2 > 0) of
                true -> % 只有VdotD为负（即v在d上的投影与d反向）的情况下，才有可能满足该条件，从而处于拦截的有利形势
                    erlang:min(I1, I2); % I1
                false ->
                    inf
            end
    end,
    {UMin, UMinColor} = case Result < Min of
        true ->
            {Result, Color};
        false ->
            {Min, MinColor}
    end,
    intr(Gaze, Objects, {UMin, UMinColor});
intr(Gaze, [{line, _Id, Color, _Pivot, [{X1, Y1}, {X2, Y2}], _Parameter}|Objects], {Min, MinColor}) ->
    {S, D} = Gaze, % 自己的位置和朝向
    {Xs, Ys} = S, % 自己的位置
    {Xd, Yd} = D, % 自己的朝向，单位矢量
    PerpX0 = Yd, % 和自己的朝向垂直（顺时针90度旋转）的等长矢量的x分量
    PerpY0 = -Xd, % 和自己的朝向垂直（顺时针90度旋转）的等长矢量的y分量
    PerpX1 = Y2 - Y1, % 和部件线垂直（顺时针90度旋转）的等长矢量的x分量
    PerpY1 = -(X2 - X1), % 和部件线垂直（顺时针90度旋转）的等长矢量的y分量
    Result = case PerpX1 * Xd + PerpY1 * Yd of
        0.0 -> % 点积为0表示垂直，即Gaze方向与目标直线平行
            inf;
        Denom -> % 分母，设和部件线垂直（顺时针90度旋转）的等长矢量为l，则把d与l的点积（理解为在l上投影）作为分母
            RayLength = ((PerpX1 * (X1 - Xs)) + (PerpY1 * (Y1 - Ys))) / Denom, % 自己到部件线端点1的矢量与l的点积（理解为在l上投影），并以分母为单位标准化，视为自己到部件线的距离
            T = ((PerpX0 * (X1 - Xs)) + (PerpY0 * (Y1 - Ys))) / Denom, % 自己到部件线端点1的矢量在d的垂直方向（顺时针90度旋转）上的投影，并以分母为单位标准化
            case (RayLength >= 0) andalso (T >= 0) andalso (T =< 1) of
                true -> % d的方向上相向而行，与d垂直的方向上目标也在朝自己靠近
                    RayLength;
                false ->
                    inf
            end
    end,
    {UMin, UMinColor} = case Result < Min of
        true ->
            {Result, Color};
        false ->
            {Min, MinColor}
    end,
    intr(Gaze, Objects, {UMin, UMinColor});
intr(_Gaze, [], {Min, MinColor}) ->
    {Min, MinColor}.

clr2val(Color) ->
    ?DBG("transducers:clr2val(Color): Color = ~p~n", [Color]),
    case Color of
        black -> -1; % poison
        cyan -> -0.75;
        green -> -0.5; % plant
        yellow -> -0.25;
        blue -> 0; % prey
        grey -> 0.25;
        red -> 0.5; % predator
        brown -> 0.75; % wall
        _ -> 1 % emptiness
    end.

val2clr(-1) -> black;
val2clr(-0.75) -> cyan;
val2clr(-0.5) -> green;
val2clr(-0.25) -> yellow;
val2clr(0) -> blue;
val2clr(0.25) -> grey;
val2clr(0.5) -> red;
val2clr(0.75) -> brown;
val2clr(_) -> white.

