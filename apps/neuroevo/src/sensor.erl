-module(sensor).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

% When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial state message.
prep(ExoSelf_PId) ->
    receive
        {ExoSelf_PId, {Id, Cx_PId, Scape, SensorName, VL, Parameters, Fanout_PIds}} ->
            loop(Id, ExoSelf_PId, Cx_PId, Scape, SensorName, VL, Parameters, Fanout_PIds)
    end.

% The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be triggered
% to begin gathering sensory data based on its sensory role, or terminate if the cortex requests so.
loop(Id, ExoSelf_PId, Cx_PId, Scape, SensorName, VL, Parameters, Fanout_PIds) ->
    receive
        {Cx_PId, sync} ->
            case catch SensorName:sense(ExoSelf_PId, VL, Parameters, Scape) of
                {'EXIT', {timeout, _}} ->
                    ?DBG("Sensor:~p timeout, is terminating.~n", [Id]),
                    ExoSelf_PId ! {self(), stuck},
                    ok;
                {'EXIT', Reason} ->
                    ?DBG("Sensor:~p error, ~p.~n", [Id, Reason]),
                    [Pid ! {self(), forward, lists:duplicate(VL, -1)} || Pid <- Fanout_PIds],
                    loop(Id, ExoSelf_PId, Cx_PId, Scape, SensorName, VL, Parameters, Fanout_PIds);
                SensoryVector ->
                    [Pid ! {self(), forward, SensoryVector} || Pid <- Fanout_PIds],
                    loop(Id, ExoSelf_PId, Cx_PId, Scape, SensorName, VL, Parameters, Fanout_PIds)
            end;
        {ExoSelf_PId, terminate} ->
            ?DBG("Sensor:~p is terminating.~n", [Id]),
            ok
    end.

