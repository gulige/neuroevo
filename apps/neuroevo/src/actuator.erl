-module(actuator).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

% When gen/2 is executed it spawns the actuator element and immediately begins to wait for its initial state message.
prep(ExoSelf_PId) ->
    receive
        {ExoSelf_PId, {Id, Cx_PId, Scape, ActuatorName, VL, Parameters, Fanin_PIds}} ->
            loop(Id, ExoSelf_PId, Cx_PId, Scape, ActuatorName, VL, Parameters, {Fanin_PIds, Fanin_PIds}, [])
    end.

% The actuator process gathers the control signals from the neurons, appending them to the accumulator. The order in which
% the signals are accumulated into a vector is in the same order as the neuron ids are stored within NIds. Once all the signals
% have been gathered, the actuator sends cortex the sync signal, executes its function, and then again begins to wait for
% the neural signals from the output layer by reseting the Fanin_PIds from the second copy of the list.
loop(Id, ExoSelf_PId, Cx_PId, Scape, ActuatorName, VL, Parameters, {[From_PId|Fanin_PIds], MFanin_PIds}, Acc) ->
    receive
        {From_PId, forward, Input} ->
            loop(Id, ExoSelf_PId, Cx_PId, Scape, ActuatorName, VL, Parameters, {Fanin_PIds, MFanin_PIds}, lists:append(Input, Acc));
        {ExoSelf_PId, terminate} ->
            ?DBG("Actuator:~p is terminating.~n", [self()]),
            ok
    end;
loop(Id, ExoSelf_PId, Cx_PId, Scape, ActuatorName, VL, Parameters, {[], MFanin_PIds}, Acc) ->
    case catch ActuatorName:act(ExoSelf_PId, lists:reverse(Acc), VL, Parameters, Scape) of
        {'EXIT', {timeout, _}} ->
            ?DBG("Actuator:~p timeout, is terminating.~n", [Id]),
            ExoSelf_PId ! {self(), stuck},
            ok;
        {'EXIT', Reason} ->
            ?DBG("Actuator:~p error, ~p.~n", [Id, Reason]),
            Cx_PId ! {self(), sync, 0, 0},
            loop(Id, ExoSelf_PId, Cx_PId, Scape, ActuatorName, VL, Parameters, {MFanin_PIds, MFanin_PIds}, []);
        {Fitness, EndFlag} ->
            Cx_PId ! {self(), sync, Fitness, EndFlag},
            loop(Id, ExoSelf_PId, Cx_PId, Scape, ActuatorName, VL, Parameters, {MFanin_PIds, MFanin_PIds}, [])
    end.

