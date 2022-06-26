%% 基底CPP（Coordinate Pre-Processor）

-module(substrate_cpp).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

% When gen/2 is executed, it spawns the substrate_cpp element and immediately begins to wait for its initial state message.
gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf_PId) ->
    receive
        {ExoSelf_PId, {Id, Cx_PId, Substrate_PId, CPPName, VL, Parameters, Fanout_PIds}} ->
            loop(Id, ExoSelf_PId, Cx_PId, Substrate_PId, CPPName, VL, Parameters, Fanout_PIds)
    end.

loop(Id, ExoSelf_PId, Cx_PId, Substrate_PId, CPPName, VL, Parameters, Fanout_PIds) ->
    receive
        {Substrate_PId, Presynaptic_Coords, Postsynaptic_Coords} ->
            SensoryVector = functions_cpp:CPPName(Presynaptic_Coords, Postsynaptic_Coords),
            [Pid ! {self(), forward, SensoryVector} || Pid <- Fanout_PIds],
            loop(Id, ExoSelf_PId, Cx_PId, Substrate_PId, CPPName, VL, Parameters, Fanout_PIds);
        {Substrate_PId, Presynaptic_Coords, Postsynaptic_Coords, IOW} -> % [I,O,W]
            SensoryVector = functions_cpp:CPPName(Presynaptic_Coords, Postsynaptic_Coords, IOW),
            ?DBG("SensoryVector:~p~n", [SensoryVector]),
            [Pid ! {self(), forward, SensoryVector} || Pid <- Fanout_PIds],
            loop(Id, ExoSelf_PId, Cx_PId, Substrate_PId, CPPName, VL, Parameters, Fanout_PIds);
        {ExoSelf_PId, terminate} ->
            ?DBG("substrate_cpp:~p is terminating.~n", [Id]),
            ok
    end.

