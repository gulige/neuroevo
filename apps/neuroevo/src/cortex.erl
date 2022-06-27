-module(cortex).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

-record(state, {
    id,
    exoself_pid,
    spids,
    apids,
    npids,
    cycle_acc = 0,
    fitness_acc = 0,
    endflag = 0,
    status
}).

gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

% The gen/2 function spawns the cortex element, which immediately starts to wait for a the state message from the same process that spawned it, exoself.
% The initial state message contains the sensor, actuator, and neuron PId lists. The message also specifies how many total Sense-Think-Act cycles
% the Cortex should execute before terminating the NN system. Once we implement the learning algorithm, the termination criteria will depend on
% the fitness of the NN, or some other useful property
prep(ExoSelf_PId) ->
    rand:seed(exs64, util:now()),
    receive
        {ExoSelf_PId, Id, SPIds, NPIds, APIds} ->
            put(start_time, util:now()),
            [SPId ! {self(), sync} || SPId <- SPIds],
            loop(Id, ExoSelf_PId, SPIds, {APIds, APIds}, NPIds, 1, 0, 0, active)
    end.

% The cortex's goal is to synchronize the the NN system such that when the actuators have received all their control signals,
% the sensors are once again triggered to gather new sensory information. Thus the cortex waits for the sync messages
% from the actuator PIds in its system, and once it has received all the sync messages, it triggers the sensors and then
% drops back to waiting for a new set of sync messages. The cortex stores 2 copies of the actuator PIds: the APIds,
% and the MemoryAPIds (MAPIds). Once all the actuators have sent it the sync messages, it can restore the APIds list from the MAPIds.
% Finally, there is also the Step variable which decrements every time a full cycle of Sense-Think-Act completes,
% once this reaches 0, the NN system begins its termination and backup process.
loop(Id, ExoSelf_PId, SPIds, {[APId|APIds], MAPIds}, NPIds, CycleAcc, FitnessAcc, EFAcc, active) ->
    receive
        {APId, sync, Fitness, EndFlag} ->
            %?DBG("Fitness:~p~n", [Fitness]),
            case Fitness =:= goal_reached of
                true ->
                    put(goal_reached, true),
                    cortex:loop(Id, ExoSelf_PId, SPIds, {APIds, MAPIds}, NPIds, CycleAcc, FitnessAcc, EFAcc + EndFlag, active);
                false ->
                    cortex:loop(Id, ExoSelf_PId, SPIds, {APIds, MAPIds}, NPIds, CycleAcc, FitnessAcc + Fitness, EFAcc + EndFlag, active)
            end;
        terminate -> % 不会收到这个消息
            ?DBG("Cortex:~p is terminating.~n", [Id]),
            [PId ! {self(), terminate} || PId <- SPIds],
            [PId ! {self(), terminate} || PId <- MAPIds],
            [PId ! {self(), terminate} || PId <- NPIds]
    end;
loop(Id, ExoSelf_PId, SPIds, {[], MAPIds}, NPIds, CycleAcc, FitnessAcc, EFAcc, active) ->
    case EFAcc > 0 of
        true -> % Organism finished evaluation（即使某个Acuator设了终止标记，也必须等到所有的Acuator都发了sync）
            TimeDif = timer:now_diff(util:now(), get(start_time)),
            % CycleAcc：本组权重评估中训练轮数累积（每一轮训练对应一次sense-think-act过程）
            ExoSelf_PId ! {self(), evaluation_completed, FitnessAcc, CycleAcc, TimeDif, get(goal_reached)},
            % 一组权重评估完成后，进入inactive状态
            cortex:loop(Id, ExoSelf_PId, SPIds, {MAPIds, MAPIds}, NPIds, CycleAcc, FitnessAcc, EFAcc, inactive);
        false ->
            [SPId ! {self(), sync} || SPId <- SPIds],
            cortex:loop(Id, ExoSelf_PId, SPIds, {MAPIds, MAPIds}, NPIds, CycleAcc + 1, FitnessAcc, EFAcc, active)
    end;
loop(Id, ExoSelf_PId, SPIds, {MAPIds, MAPIds}, NPIds, _CycleAcc, _FitnessAcc, _EFAcc, inactive) ->
    receive
        {ExoSelf_PId, reactivate} -> % 等待权重扰动后被再次激活
            put(start_time, util:now()),
            [SPId ! {self(), sync} || SPId <- SPIds],
            cortex:loop(Id, ExoSelf_PId, SPIds, {MAPIds, MAPIds}, NPIds, 1, 0, 0, active);
        {ExoSelf_PId, terminate} ->
            ?DBG("Cortex:~p is terminating.~n", [Id]),
            ok
    end.

