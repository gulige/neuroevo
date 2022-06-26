-module(xor_sim).
-export([run/1]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

run(ExoSelf_PId) ->
    XOR = [{[-1, -1], [-1]}, {[1, -1], [1]}, {[-1, 1], [1]}, {[1, 1], [-1]}],
    run(ExoSelf_PId, {XOR, XOR}, 0).

run(ExoSelf_PId, {[{Input, CorrectOutput}|XOR], MXOR}, ErrAcc) ->
    receive
        {From, sense} ->
            From ! {self(), percept, Input},
            run(ExoSelf_PId, {[{Input, CorrectOutput}|XOR], MXOR}, ErrAcc);
        {From, action, Output} ->
            Error = sse(Output, CorrectOutput, 0),
            case XOR of
                [] ->
                    SSE = ErrAcc + Error,
                    Fitness = 1 / (SSE + 0.000001),
                    From ! {self(), Fitness, 1}, % 1：EndFlag
                    run(ExoSelf_PId, {MXOR, MXOR}, 0);
                _ ->
                    From ! {self(), 0, 0},
                    run(ExoSelf_PId, {XOR, MXOR}, ErrAcc + Error)
            end;
        {ExoSelf_PId, terminate} ->
            ok
    end.

% 误差平方和
sse([O|Output], [T|Target], SSE) ->
    SE = math:pow(T - O, 2),
    sse(Output, Target, SE + SSE);
sse([], [], SSE) ->
    SSE.

