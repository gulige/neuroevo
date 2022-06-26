%% 基底CEP（Connectivity Expression Producer）

-module(substrate_cep).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

% When gen/2 is executed it spawns the actuator element and immediately begins to wait for its initial state message.
gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf_PId) ->
    receive
        {ExoSelf_PId, {Id, Cx_PId, Substrate_PId, CEPName, Parameters, Fanin_PIds}} ->
            loop(Id, ExoSelf_PId, Cx_PId, Substrate_PId, CEPName, Parameters, {Fanin_PIds, Fanin_PIds}, [])
    end.

% The substrate_cep process gathers the control signals from the neurons, appending them to the accumulator. The order in which
% the signals are accumulated into a vector is in the same order that the neuron ids are stored within NIds. Once all the signals
% have been gathered, the substrate_cep executes its function, forwards the processed signal to the substrate, and then again
% begins to wait for the neural signals from the output layer by reseting the Fanin_PIds from the second copy of the list.
loop(Id, ExoSelf_PId, Cx_PId, Substrate_PId, CEPName, Parameters, {[From_PId|Fanin_PIds], MFanin_PIds}, Acc) ->
    receive
        {From_PId, forward, Input} ->
            loop(Id, ExoSelf_PId, Cx_PId, Substrate_PId, CEPName, Parameters, {Fanin_PIds, MFanin_PIds}, lists:append(Input, Acc));
        {ExoSelf_PId, terminate} ->
            ?DBG("Substrate_CEP:~p is terminating.~n", [self()]),
            ok
    end;
loop(Id, ExoSelf_PId, Cx_PId, Substrate_PId, CEPName, Parameters, {[], MFanin_PIds}, Acc) ->
    ProperlyOrdered_Input = lists:reverse(Acc),
    functions_cep:CEPName(ProperlyOrdered_Input, Parameters, Substrate_PId),
    loop(Id, ExoSelf_PId, Cx_PId, Substrate_PId, CEPName, Parameters, {MFanin_PIds, MFanin_PIds}, []).

