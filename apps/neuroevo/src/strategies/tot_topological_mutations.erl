%% 用于计算需要发生多少次拓扑结构的变异

-module(tot_topological_mutations).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

% ncount_exponential/2 calculates TotMutations by putting the size of the NN to some power Power.
% 计算需要发生多少次拓扑结构的变异：基于agent神经元数量的幂关系，并有随机性
ncount_exponential(Power, Agent_Id) ->
    A = genotype:read({agent, Agent_Id}),
    Cx = genotype:read({cortex, A#agent.cx_id}),
    TotNeurons = length(Cx#cortex.neuron_ids),
    TotMutations = rand:uniform(round(math:pow(TotNeurons, Power))),
    ?DBG("Tot neurons:~p Performing Tot mutations:~p on:~p~n", [TotNeurons, TotMutations, Agent_Id]),
    TotMutations.

% ncount_linear/2 calcualtes TotMutations by multiplying the size of the NN by the value Multiplier.
% 计算需要发生多少次拓扑结构的变异：基于agent神经元数量的倍数关系，没有随机性
ncount_linear(Multiplier, Agent_Id) ->
    A = genotype:read({agent, Agent_Id}),
    Cx = genotype:read({cortex, A#agent.cx_id}),
    TotNeurons = length(Cx#cortex.neuron_ids),
    TotMutations = TotNeurons * Multiplier,
    ?DBG("Tot neurons:~p Performing Tot mutations:~p on:~p~n", [TotNeurons, TotMutations, Agent_Id]),
    TotMutations.

