-module(functions).
-compile(export_all).

% 饱和，将Val钳制在正负Spread之间
saturation(Val, Spread) when Spread > 0 ->
    if
        Val < -Spread -> -Spread;
        Val > Spread -> Spread;
        true -> Val
    end.

% sat/3 calculates whether Val is between Min and Max values, if it is, then val is returned as is. If Val is less than Min, then Min is returned,
% if Val is greater than Max, then Max is returned.
sat(Val, Max, Min) when Max > Min ->
    if
        Val < Min -> Min;
        Val > Max -> Max;
        true -> Val
    end.

% 与sat的区别在于：DZMin和DZMax之间的取值为0
sat_dzone(Val, Max, Min, DZMax, DZMin) when Max > DZMax, DZMax > DZMin, DZMin > Min ->
    case (Val < DZMax) andalso (Val > DZMin) of
        true -> 0;
        false -> sat(Val, Max, Min)
    end.

% Val距离平均位置(Max+Min)/2的差与振幅(Max-Min)/2的比值
% 将 [Min, Max] 映射到 [-1.0, 1.0]时，Val所处的位置
scale([_|_] = L, Max, Min) ->
    [scale(Val, Max, Min) || Val <- L];
scale(Val, Max, Min) when Max >= Min ->
    case Max == Min of
        true -> 0;
        false -> (Val * 2 - (Max + Min)) / (Max - Min)
    end.

% 按Val在MaxMagnitude和Threshold下的scale比例，直接在0到MaxMagnitude的范围内放大后的位置
% 将 [Min, Max] 映射到 [0, Max] 或 [-Max, 0]时，Val所处的位置
scale_dzone(Val, Threshold, MaxMagnitude)
  when Threshold > 0, MaxMagnitude > 0, MaxMagnitude > Threshold ->
    if 
        Val > Threshold -> % (0, Max]
            (scale(Val, MaxMagnitude, Threshold) + 1) * MaxMagnitude / 2;
        Val < -Threshold -> % [-Max, 0)
            (scale(Val, -Threshold, -MaxMagnitude) - 1) * MaxMagnitude / 2;
        true ->
            0
    end.

% 算术平均值
avg(List) ->
    lists:sum(List) / length(List).

% 标准差（均方差）
std(List) ->
    Avg = avg(List),
    std(List, Avg, []).

std([Val|List], Avg, Acc) ->
    std(List, Avg, [math:pow(Val - Avg, 2)|Acc]);
std([], _Avg, Acc) ->
    Variance = lists:sum(Acc) / length(Acc), % 方差
    math:sqrt(Variance).

%%====================================================================
%% CPP坐标预处理使用
%%====================================================================

% cartesian2polar
% Theta: 0-2Pi, R: 0+
cart2pol({X, Y}) ->
    R = math:sqrt(X * X + Y * Y),
    Theta = case R == 0 of
        true -> 0;
        false ->
            if
                (X > 0) andalso (Y >= 0) -> math:atan(Y / X);
                (X > 0) andalso (Y < 0)  -> math:atan(Y / X) + 2 * math:pi();
                (X < 0)                  -> math:atan(Y / X) + math:pi();
                (X == 0) andalso (Y > 0) -> math:pi() / 2;
                (X == 0) andalso (Y < 0) -> 3 * math:pi() / 2
            end
    end,
    {R, Theta}.

% cartesian2spherical
% Theta: 0-2Pi, Phi:0-Pi, R: 0+, P: 0+
cart2spher({X, Y, Z}) ->
    PreR = X * X + Y * Y,
    R = math:sqrt(PreR),
    P = math:sqrt(PreR + Z * Z),
    Theta = case R == 0 of
        true -> 0;
        false ->
            if
                (X > 0) andalso (Y >= 0) -> math:atan(Y / X);
                (X > 0) andalso (Y < 0)  -> math:atan(Y / X) + 2 * math:pi();
                (X < 0)                  -> math:atan(Y / X) + math:pi();
                (X == 0) andalso (Y > 0) -> math:pi() / 2;
                (X == 0) andalso (Y < 0) -> 3 * math:pi() / 2
            end
    end,
    Phi = case P == 0 of
        true -> 0;
        false -> math:acos(Z / P)
    end,
    {P, Theta, Phi}.

% polar2cartesian
pol2cart({R, Theta}) ->
    X = R * math:cos(Theta),
    Y = R * math:sin(Theta),
    {X, Y}.

% spherical2cartesian
spher2cart({P, Theta, Phi}) ->
    X = P * math:sin(Phi) * math:cos(Theta),
    Y = P * math:sin(Phi) * math:sin(Theta),
    Z = P * math:cos(Phi),
    {X, Y, Z}.

to_cartesian(Direction) ->
    case Direction of
        {spherical, Coordinates} ->
            {cartesian, spher2cart(Coordinates)};
        {polar, Coordinates} ->
            {cartesian, pol2cart(Coordinates)};
        {cartesian, Coordinates} ->
            {cartesian, Coordinates}
    end.

%%====================================================================
%% CPP坐标预处理使用 (END)
%%====================================================================

%--> 取向量的单位矢量
normalize(Vector) ->
    Normalizer = calculate_normalizer(Vector, 0),
    normalize(Vector, Normalizer, []).

% 计算向量长度
calculate_normalizer([Val|Vector], Acc) ->
    calculate_normalizer(Vector, Val * Val + Acc);
calculate_normalizer([], Acc) ->
    math:sqrt(Acc).

% 归一化
normalize([Val|Vector], Normalizer, Acc) ->
    normalize(Vector, Normalizer, [Val / Normalizer|Acc]);
normalize([], _Normalizer, Acc) ->
    lists:reverse(Acc).

%--> 计算两个坐标点间的距离
distance(Vector1, Vector2) ->
    distance(Vector1, Vector2, 0).

distance([Val1|Vector1], [Val2|Vector2], Acc) ->
    distance(Vector1, Vector2, Acc + math:pow(Val2 - Val1, 2));
distance([], [], Acc) ->
    math:sqrt(Acc).

%--> 计算两个向量的差：V1 -> V2
vector_difference(Vector1, Vector2) ->
    vector_difference(Vector1, Vector2, []).

vector_difference([Val1|Vector1], [Val2|Vector2], Acc) ->
    vector_difference(Vector1, Vector2, [Val2 - Val1|Acc]);
vector_difference([], [], Acc) ->
    lists:reverse(Acc).

