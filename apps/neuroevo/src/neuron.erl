-module(neuron).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

-define(DELTA_MULTIPLIER, math:pi() * 2).
-define(SAT_LIMIT, math:pi() * 2).
-define(RO_SIGNAL, 0).

-record(state, {
    id, % 神经元id
    cx_pid, % cortex进程id
    af, % 激活函数
    pf, % 可塑性函数
    aggrf, % 聚合函数
    heredity_type, % 遗传类型
    si_pids = [], % 标准输入的进程id列表
    si_pidps_bl = [], % 学习前的标准输入的{进程id,权重}列表，bl - Before Learning，在刚刚扰动后还没有可塑性学习前保存的权重
    si_pidps_current = [], % 当前的标准输入的{进程id,权重}列表
    si_pidps_backup = [], % 备份的标准输入的{进程id,权重}列表
    mi_pids = [], % 调制输入的进程id列表
    mi_pidps_current = [], % 当前的调制输入的{进程id,权重}列表
    mi_pidps_backup = [], % 备份的调制输入的{进程id,权重}列表
    output_pids = [], % 输出的进程id列表
    ro_pids = [] % 循环输出的进程id列表
}).

gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

% When gen/2 is executed, it spawns the neuron element and immediately begins to wait for its initial state message from the exoself.
% Once the state message arrives, the neuron sends out the default forward signals to any elements in its ro_ids list, if any.
% Afterwards, prep drops into the neuron's main loop.
prep(ExoSelf_PId) ->
    rand:seed(exs64, util:now()),
    receive
        {ExoSelf_PId, {Id, Cx_PId, AF, PF, AggrF, HeredityType, SI_PIdPs, MI_PIdPs, Output_PIds, RO_PIds}} ->
            fanout(RO_PIds, {self(), forward, [?RO_SIGNAL]}),
            SI_PIds = lists:append([IPId || {IPId, _W} <- SI_PIdPs, IPId =/= bias], [ok]),
            MI_PIds = lists:append([IPId || {IPId, _W} <- MI_PIdPs, IPId =/= bias], [ok]),
            S = #state{
                id = Id,
                cx_pid = Cx_PId,
                af = AF,
                pf = PF,
                aggrf = AggrF,
                heredity_type = HeredityType,
                si_pids = SI_PIds,
                si_pidps_bl = SI_PIdPs,
                si_pidps_current = SI_PIdPs,
                si_pidps_backup = SI_PIdPs,
                mi_pids = MI_PIds,
                mi_pidps_current = MI_PIdPs,
                mi_pidps_backup = MI_PIdPs,
                output_pids = Output_PIds,
                ro_pids = RO_PIds
            },
            loop(S, ExoSelf_PId, SI_PIds, MI_PIds, [], [])
    end.

% The neuron process waits for vector signals from all the processes that it's connected from, taking the dot product of the input and weight vectors,
% and then adding it to the accumulator. Once all the signals from Input_PIds are received, the accumulator contains the dot product to which the neuron
% then adds the bias and executes the activation function. After fanning out the output signal, the neuron again returns to waiting for incoming signals.
% When the neuron receives the {ExoSelf_PId,get_backup} message, it forwards to the exoself its full MInput_PIdPs list, and its Id.
% The MInput_PIdPs contains the modified, tuned and most effective version of the input_idps. The neuron process is also accepts weight_backup signal,
% when receiving it the neuron saves to process dictionary the current MInput_PIdPs. When the neuron receives the weight_restore signal, it reads back
% from the process dictionary the stored Input_PIdPs, and switches over to using it as its active Input_PIdPs list. When the neuron receives the
% weight_perturb signal from the exoself, it perturbs the weights by executing the perturb_Lipids/1 function, which returns the updated list.
% Finally, the neuron can also accept a reset_prep signal, which makes the neuron flush its buffer in the off chance that it has a recursively sent signal
% in its inbox. After flushing its buffer, the neuron waits for the exoself to send it the reset signal, at which point the neuron, now fully refreshed
% after the flush_buffer/0, outputs a default forward signal to its recursively connected elements, if any, and then drops back into the main loop.
loop(S, ExoSelf_PId, [ok], [ok], SIAcc, MIAcc) ->
    PF = S#state.pf,
    AF = S#state.af,
    AggrF = S#state.aggrf,
    {PFName, PFParameters} = PF,
    Ordered_SIAcc = lists:reverse(SIAcc),
    SI_PIdPs = S#state.si_pidps_current,
    SAggregation_Product = functions:saturation(signal_aggregator:AggrF(Ordered_SIAcc, SI_PIdPs), ?SAT_LIMIT),
    SOutput = activation_functions:AF(SAggregation_Product),
    Output_PIds = S#state.output_pids,
    [Output_PId ! {self(), forward, [SOutput]} || Output_PId <- Output_PIds],
    U_S = case PFName of
        none -> S;
        _ -> % 进入可塑性处理
            Ordered_MIAcc = lists:reverse(MIAcc),
            MI_PIdPs = S#state.mi_pidps_current,
            MAggregation_Product = functions:saturation(signal_aggregator:dot_product(Ordered_MIAcc, MI_PIdPs), ?SAT_LIMIT),
            MOutput = activation_functions:tanh(MAggregation_Product), % 固定是tanh
            U_SI_PIdPs = plasticity:PFName([MOutput|PFParameters], Ordered_SIAcc, SI_PIdPs, SOutput), % 可塑性对权重的调整（根据输出）
            % 下一轮继续逼近它所能达到的局部最优，可塑性权重调整和权重扰动不同，扰动是“无意识”、“无导向”、随机扩展局部最优的探索空间
            S#state{si_pidps_current = U_SI_PIdPs}
    end,
    SI_PIds = S#state.si_pids,
    MI_PIds = S#state.mi_pids,
    neuron:loop(U_S, ExoSelf_PId, SI_PIds, MI_PIds, [], []); % 收集齐了处理完了再进行收集
loop(S, ExoSelf_PId, [SI_PId|SI_PIds], [MI_PId|MI_PIds], SIAcc, MIAcc) ->
    receive
        {SI_PId, forward, Input} ->
            neuron:loop(S, ExoSelf_PId, SI_PIds, [MI_PId|MI_PIds], [{SI_PId, Input}|SIAcc], MIAcc);
        {MI_PId, forward, Input} ->
            neuron:loop(S, ExoSelf_PId, [SI_PId|SI_PIds], MI_PIds, SIAcc, [{MI_PId, Input}|MIAcc]);
        {ExoSelf_PId, weight_backup} ->
            U_S = case S#state.heredity_type of
                darwinian ->
                    S#state{
                        si_pidps_backup = S#state.si_pidps_bl,
                        mi_pidps_backup = S#state.mi_pidps_current
                    };
                lamarckian ->
                    S#state{
                        si_pidps_backup = S#state.si_pidps_current,
                        mi_pidps_backup = S#state.mi_pidps_current
                    }
            end,
            neuron:loop(U_S, ExoSelf_PId, [SI_PId|SI_PIds], [MI_PId|MI_PIds], SIAcc, MIAcc);
        {ExoSelf_PId, weight_restore} ->
            U_S = S#state{
                si_pidps_bl = S#state.si_pidps_backup,
                si_pidps_current = S#state.si_pidps_backup,
                mi_pidps_current = S#state.mi_pidps_backup
            },
            neuron:loop(U_S, ExoSelf_PId, [SI_PId|SI_PIds], [MI_PId|MI_PIds], SIAcc, MIAcc);
        {ExoSelf_PId, weight_perturb, Spread} ->
            Perturbed_SIPIdPs = perturb_IPIdPs(Spread, S#state.si_pidps_backup),
            Perturbed_MIPIdPs = perturb_IPIdPs(Spread, S#state.mi_pidps_backup),
            U_S = S#state{
                si_pidps_bl = Perturbed_SIPIdPs,
                si_pidps_current = Perturbed_SIPIdPs,
                mi_pidps_current = Perturbed_MIPIdPs
            },
            neuron:loop(U_S, ExoSelf_PId, [SI_PId|SI_PIds], [MI_PId|MI_PIds], SIAcc, MIAcc);
        {ExoSelf_PId, reset_prep} ->
            flush_buffer(),
            ExoSelf_PId ! {self(), ready},
            RO_PIds = S#state.ro_pids,
            receive
                {ExoSelf_PId, reset} ->
                    fanout(RO_PIds, {self(), forward, [?RO_SIGNAL]})
            end,
            neuron:loop(S, ExoSelf_PId, S#state.si_pids, S#state.mi_pids, [], []);
        {ExoSelf_PId, get_backup} ->
            NId = S#state.id,
            ExoSelf_PId ! {self(), NId, S#state.si_pidps_backup, S#state.mi_pidps_backup},
            neuron:loop(S, ExoSelf_PId, [SI_PId|SI_PIds], [MI_PId|MI_PIds], SIAcc, MIAcc);
        {ExoSelf_PId, terminate} ->
            ?DBG("Neuron:~p is terminating.~n", [self()]),
            ok
    after 10000 ->
        ?ERR("neuron:~p stuck.~n", [S#state.id])
    end.

% The fanout/2 function fans out the Msg to all the PIds in its list.
fanout([Pid|Pids], Msg) ->
    Pid ! Msg,
    fanout(Pids, Msg);
fanout([], _Msg) ->
    done.

% The flush_buffer/0 cleans out the element's inbox.
flush_buffer() ->
    receive
        _ ->
            flush_buffer()
    after 0 ->
        done
end.

% The perturb_IPIdPs/2 function calculates the probability with which each neuron in the Input_PIdPs is chosen to be perturbed.
% The probablity is based on the total number of weights in the Input_PIdPs list, with the actual mutation probablity equating to
% the inverse of square root of total number of weights. The perturb_IPIdPs/4 function goes through each weights block and
% calls the perturb_weights/4 to perturb the weights.
perturb_IPIdPs(Spread, []) -> [];
perturb_IPIdPs(Spread, Input_PIdPs) ->
    Tot_Weights = lists:sum([length(WeightsP) || {_Input_PId, WeightsP} <- Input_PIdPs]),
    MP = 1 / math:sqrt(Tot_Weights),
    perturb_IPIdPs(Spread, MP, Input_PIdPs, []).

perturb_IPIdPs(Spread, MP, [{Input_PId, WeightsP}|Input_PIdPs], Acc) ->
    U_WeightsP = perturb_weightsP(Spread, MP, WeightsP, []),
    perturb_IPIdPs(Spread, MP, Input_PIdPs, [{Input_PId, U_WeightsP}|Acc]);
perturb_IPIdPs(_Spread, _MP, [], Acc) ->
    lists:reverse(Acc).

% The perturb_weights/4 function is the function that actually goes through each weight block, and perturbs each weight with a probablity of MP.
% If the weight is chosen to be perturbed, the perturbation intensity is chosen uniformly between -Spread and Spread.
perturb_weightsP(Spread, MP, [{W, LPs}|WeightsP], Acc) ->
    U_W = case rand:uniform() < MP of
        true ->
            % 对W进行-Spread到Spread的随机扰动
            functions:saturation((rand:uniform() - 0.5) * 2 * Spread + W, ?SAT_LIMIT);
        false ->
            W
    end,
    perturb_weightsP(Spread, MP, WeightsP, [{U_W, LPs}|Acc]);
perturb_weightsP(_Spread, _MP, [], Acc) ->
    lists:reverse(Acc).

