%% 用于选择将被扰动的神经元列表

-module(tuning_selection).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

%% dynamic、dynamic_random、active、active_random、current、current_random、all、all_random

% 在1与无穷大之间随机（一定的概率分布）选择AgeLimit（75%的概率<=2，11%的概率>=3），然后提取比Generation年轻AgeLimit代以内（包括）的神经元IdP列表
% The dynamic/4 selection function randomly selects an age limit for its neuron id pool. The age limit is chosen by executing
% math:sqrt(1/rand:uniform()), which creates a value between 1 and infinity. Using this function there is 75% that the number will be =< 2,
% 25% that it will be >= 2, 11% that it will be >= 3... Everytime this selection function is executed, the AgeLimit is generated anew,
% thus different times it will produce different neuron id pools for tuning.
dynamic(N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
    AgeLimit = math:sqrt(1 / rand:uniform()),
    ChosenN_IdPs = case extract_CurGenNIdPs(N_Ids, AgentGeneration, AgeLimit, PerturbationRange, AnnealingParameter, []) of
        [] -> % 没找到符合条件的，则拿第一个充数
            [N_Id|_] = N_Ids,
            [{N_Id, PerturbationRange * math:pi()}];
        ExtractedN_IdPs ->
            ExtractedN_IdPs
    end,
    ChosenN_IdPs.

% 在dynamic的基础上，再按1/sqrt(N)的概率随机选出神经元IdP列表
% dynamic_random/4 selection function composes the neuron id pool the same way as the dynamic/4 selection function, but after this id pool
% is generated, this selection function extracts ids from it randomly with a probability of 1/math:sqrt(Tot_Neurons). Thus the probability of
% a neuron being selected from this pool is proportional to the number of ids in that pool. If through chance no ids are selected, then
% the first element in the id pool is automatically selected, and given the highest spread.
dynamic_random(N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
    AgeLimit = math:sqrt(1 / rand:uniform()),
    ChosenN_IdPs = case extract_CurGenNIdPs(N_Ids, AgentGeneration, AgeLimit, PerturbationRange, AnnealingParameter, []) of
        [] -> % 没找到符合条件的，则拿第一个充数
            [N_Id|_] = N_Ids,
            [{N_Id, PerturbationRange * math:pi()}];
        ExtractedN_IdPs ->
            ExtractedN_IdPs
    end,
    %?DBG("ChosenN_IdPs:~p~n", [ChosenN_IdPs]),
    Tot_Neurons = length(ChosenN_IdPs),
    MutationP = 1 / math:sqrt(Tot_Neurons),
    choose_randomNIdPs(MutationP, ChosenN_IdPs).

% 提取比Generation年轻3代以内（包括）的神经元IdP列表
% active/4 selection algorithm composes a neuron id pool from all neurons who are younger than 3 generations.
active(N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
    extract_CurGenNIdPs(N_Ids, AgentGeneration, 3, PerturbationRange, AnnealingParameter, []).

% 在active的基础上，再按1/sqrt(N)的概率随机选出神经元IdP列表
% active_random/4 is a selection algorithm that composes an id pool by first creating a list of all neurons who are younger than 3 generations,
% and then composing a sub list from it by randomly choosing elements from this list with a probability of 1/math:sqrt(Tot_Neurons).
active_random(N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
    ChosenN_IdPs = case extract_CurGenNIdPs(N_Ids, AgentGeneration, 3, PerturbationRange, AnnealingParameter, []) of
        [] -> % 没找到符合条件的，则拿第一个充数
            [N_Id|_] = N_Ids,
            [{N_Id, PerturbationRange * math:pi()}];
        ExtractedN_IdPs ->
            ExtractedN_IdPs
    end,
    Tot_Neurons = length(ChosenN_IdPs),
    MutationP = 1 / math:sqrt(Tot_Neurons),
    choose_randomNIdPs(MutationP, ChosenN_IdPs).

% 提取Generation当代的神经元IdP列表
% current/4 is a selection algorithm that returns a list of all neurons which have been added to the NN, or affected by mutation,
% during the last generation.
current(N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
    case extract_CurGenNIdPs(N_Ids, AgentGeneration, 0, PerturbationRange, AnnealingParameter, []) of
        [] -> % 没找到符合条件的，则拿第一个充数
            [N_Id|_] = N_Ids,
            [{N_Id, PerturbationRange * math:pi()}];
        IdPs ->
            IdPs
    end.

% 在current的基础上，再按1/sqrt(N)的概率随机选出神经元IdP列表
% current_random/4 composes the list of tuples in the same way as current/4 does, but then composes a sublist by randomly selecting elements
% from that list with a probability of 1/math:sqrt(Tot_Neurons), and returning that to the caller.
current_random(N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
    ChosenN_IdPs = case extract_CurGenNIdPs(N_Ids, AgentGeneration, 0, PerturbationRange, AnnealingParameter, []) of
        [] -> % 没找到符合条件的，则拿第一个充数
            [N_Id|_] = N_Ids,
            [{N_Id, PerturbationRange * math:pi()}];
        IdPs ->
            IdPs
    end,
    Tot_Neurons = length(ChosenN_IdPs),
    MutationP = 1 / math:sqrt(Tot_Neurons),
    choose_randomNIdPs(MutationP, ChosenN_IdPs).

% 提取比Generation年轻Generation代（也就是全部）以内（包括）的神经元IdP列表
% all/4 returns a list of tuples composed of all ids (and their spread values) belonging to the NN, to the caller.
all(N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
    extract_CurGenNIdPs(N_Ids, AgentGeneration, AgentGeneration, PerturbationRange, AnnealingParameter, []).

% 在all的基础上，再按1/sqrt(N)的概率随机选出神经元IdP列表
% 基数N越大，扰动的绝对量sqrt(N)越大，但相对占比1/sqrt(N)越少
% all_random/4 first composes a list of tuples from nids and their spreads, and then creates a sublist by choosing each element with
% a probability of 1/math:sqrt(Tot_neurons).
all_random(N_Ids, AgentGeneration, PerturbationRange, AnnealingParameter) ->
    % 不可能找不到符合条件的
    ChosenN_IdPs = extract_CurGenNIdPs(N_Ids, AgentGeneration, AgentGeneration, PerturbationRange, AnnealingParameter, []),
    Tot_Neurons = length(ChosenN_IdPs),
    MutationP = 1 / math:sqrt(Tot_Neurons),
    choose_randomNIdPs(MutationP, ChosenN_IdPs).

% 提取比Generation年轻AgeLimit代以内（包括）的神经元IdP（代越大越老，每一次拓扑变异时增1）
% The extract_CurGenNIdPs/6 composes a neuron id pool from neurons who are younger than the AgeLimit parameter. This is calculated by
% comparing the generation when they were created or touched by mutation, with that of the agent which ages with every topological
% mutation phase. Id pool accumulates not just the neurons but also the spread which will be used for the synaptic weight perturbation.
% The spread is calculated by multiplying the perturbation_range variable by math:pi(), and then multiplied by the annealing factor
% which is math:pow(AnnealingParameter, Age). Annealing parameter is less than 1, thus the greater the age of the neuron, the lower
% the Spread will be.
extract_CurGenNIdPs([N_Id|N_Ids], Generation, AgeLimit, PerturbationRange, AnnealingParameter, Acc) ->
    N = genotype:dirty_read({neuron, N_Id}),
    NeuronGen = N#neuron.generation,
    case NeuronGen >= (Generation - AgeLimit) of % i.e. Generation - NeuronGen =< AgeLimit
        true ->
            Age = Generation - NeuronGen, % Generation >= NeuronGen
            Spread = PerturbationRange * math:pi() * math:pow(AnnealingParameter, Age), % 代差越大，越稳定，扰动幅度越小
            extract_CurGenNIdPs(N_Ids, Generation, AgeLimit, PerturbationRange, AnnealingParameter, [{N_Id, Spread}|Acc]);
        false ->
            extract_CurGenNIdPs(N_Ids, Generation, AgeLimit, PerturbationRange, AnnealingParameter, Acc)
    end;
extract_CurGenNIdPs([], _Generation, _AgeLimit, _PerturbationRange, _AnnealingParameter, Acc) ->
    Acc.

% choose_randomNIdPs/2 and choose_randomNIdPs/3 accepts a mutation probability parameter and a list of tuples composed of neuron ids and
% their spreads, and then selects from this list randomly with a probability MutationP, composing a new sub list.
choose_randomNIdPs(MutationP, N_IdPs) ->
    case choose_randomNIdPs(N_IdPs, MutationP, []) of
        [] -> % 恰巧没有挨个随机出来，则直接随机挑一个
            {NId, Spread} = lists:nth(rand:uniform(length(N_IdPs)), N_IdPs),
            [{NId, Spread}];
        Acc ->
            Acc
    end.

choose_randomNIdPs([{NId, Spread}|N_IdPs], MutationP, Acc) ->
    U_Acc = case rand:uniform() < MutationP of
        true ->
            [{NId, Spread}|Acc];
        false ->
            Acc
    end,
    choose_randomNIdPs(N_IdPs, MutationP, U_Acc);
choose_randomNIdPs([], _MutationP, Acc) ->
    Acc.

