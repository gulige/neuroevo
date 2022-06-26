%%%-----------------------------------
%%% @Module  : util
%%% @Description: 公共函数
%%%-----------------------------------
-module(util).
-compile(export_all).


now() ->
    os:timestamp().

is_even(Val) ->
    (Val rem 2) =:= 0.

% The random_element/1 function accepts a list as input, and returns a single, randomly chosen element as output.
random_element(List) ->
    lists:nth(rand:uniform(length(List)), List).

