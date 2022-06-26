-module(activation_functions).
-compile(export_all).

tanh(Val) ->
    math:tanh(Val).

cos(Val) ->
    math:cos(Val).

sin(Val) ->
    math:sin(Val).

sgn(0) -> 0;
sgn(Val) ->
    case Val > 0 of
        true -> 1;
        false -> -1
    end.

bin(Val) ->
    case Val > 0 of
        true -> 1;
        false -> 0
    end.

trinary(Val) ->
    if
        (Val < 0.33) andalso (Val > -0.33) -> 0;
        Val >= 0.33 -> 1;
        Val =< -0.33 -> -1
    end.

% 多重二次曲面
multiquadric(Val) ->
    math:pow(Val * Val + 0.01, 0.5).

absolute(Val) ->
    abs(Val).

linear(Val) ->
    Val.

% 二次方程式
quadratic(Val) ->
    sgn(Val) * Val * Val.

gaussian(Val) ->
    gaussian(2.71828183, Val).

gaussian(Const, Val) ->
    V = functions:saturation(Val, 10),
    math:pow(Const, -V * V).

sqrt(Val) ->
    sgn(Val) * math:sqrt(abs(Val)).

log(Val) ->
    case Val == 0 of
        true -> 0;
        false -> sgn(Val) * math:log(abs(Val))
    end.

% (-1 : 1) -- Der:Y*(1-Y)
sigmoid(Val) ->
    V = functions:saturation(Val, 10),
    2 / (1 + math:pow(2.71828183, -V)) - 1.

% (-1 : 1) -- Der:1/((1+abs(val))*(1+abs(val)))
sigmoid1(Val) ->
    Val / (1 + abs(Val)).

