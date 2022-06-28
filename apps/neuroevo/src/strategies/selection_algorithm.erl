%% 用于淘汰选择算法

-module(selection_algorithm).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

-define(SURVIVAL_PERCENTAGE, 0.5). % 生存比例

% The competition/3 is part of the selection algorithm dubbed "competition". The function first executes calculate_alotments/4
% to calculate the number of offspring alloted for each agent in the Sorted_AgentSummaries list. The function then calculates
% the Normalizer value, which is used then used to proportionalize the alloted number of offspring to each agent, to ensure that
% the final specie size is within PopulationLimit. The function then drops into the gather_survivors/3 function which, using
% the normalized offspring allotment values, creates the actual mutant offspring.
% ProperlySorted_AgentSummaries元素结构：{Fitness, TotNeurons, AgentId}
% TopAgent_Ids是Top 3，根据前50%的agent的适应度计算各自的子孙配额，据此变异生成子孙，并且最终的总人口限制在PopulationLimit
competition(ProperlySorted_AgentSummaries, NeuralEnergyCost, PopulationLimit) ->
    TotSurvivors = round(length(ProperlySorted_AgentSummaries) * ?SURVIVAL_PERCENTAGE),
    Valid_AgentSummaries = lists:sublist(ProperlySorted_AgentSummaries, TotSurvivors),
    Invalid_AgentSummaries = ProperlySorted_AgentSummaries -- Valid_AgentSummaries,
    {_, _, Invalid_AgentIds} = lists:unzip3(Invalid_AgentSummaries),
    [genotype:delete_Agent(Agent_Id) || Agent_Id <- Invalid_AgentIds],
    %?DBG("Valid_AgentSummaries:~p~n", [Valid_AgentSummaries]),
    %?DBG("Invalid_AgentSummaries:~p~n", [Invalid_AgentSummaries]),
    TopAgentSummaries = lists:sublist(Valid_AgentSummaries, 3), % Top 3
    {_TopFitnessList, _TopTotNs, TopAgent_Ids} = lists:unzip3(TopAgentSummaries),
    %?DBG("NeuralEnergyCost:~p, PopulationLimit:~p~n", [NeuralEnergyCost, PopulationLimit]),
    {AlotmentsP, NextGenSize_Estimate} = calculate_alotments(Valid_AgentSummaries, NeuralEnergyCost, [], 0),
    Normalizer = NextGenSize_Estimate / PopulationLimit,
    %?DBG("Population size normalizer:~p~n", [Normalizer]),
    NewGenAgent_Ids = gather_survivors(AlotmentsP, Normalizer, []),
    {NewGenAgent_Ids, TopAgent_Ids}.

% The calculate_alotments/4 function accepts the AgentSummaries list and for each agent, using the NeuralEnergyCost, calcualtes
% how many offspring that agent can produce by using the agent's Fitness, TotNEurons, and NeuralEnergyCost values. The function
% first calculates how many neurons the agent is alloted, based on the agent's fitness and the cost of each neuron (which itself
% was calculated based on the average performance of the population). From the number of neurons alloted to the agent, the function
% then calculates how many offspring the agent should be alloted, by dividing the agent's NN size by the number of neurons it is alloted.
% The function also keeps track of how many offspring will be created from all these agents in general, by adding up all the offspring
% alotements. The calcualte_alotments/4 function does this for each tuple in the AgentSummaries, and then returns the calculated
% alotment list and NewPopAcc to the caller.
% 返回{AlotmentsP, NextGenSize_Estimate}，AlotmentsP元素结构：{NextGenAlotment, Fitness, TotNeurons, AgentId}
calculate_alotments([{Fitness, TotNeurons, Agent_Id}|Sorted_AgentSummaries], NeuralEnergyCost, Acc, NewPopAcc) ->
    NeuralAlotment = Fitness / NeuralEnergyCost, % 分配多少个神经元
    NextGenAlotment = NeuralAlotment / TotNeurons, % 分配多少个NN（agent）
    U_NewPopAcc = NewPopAcc + NextGenAlotment,
    calculate_alotments(Sorted_AgentSummaries, NeuralEnergyCost, [{NextGenAlotment, Fitness, TotNeurons, Agent_Id}|Acc], U_NewPopAcc);
calculate_alotments([], _NeuralEnergyCost, Acc, NewPopAcc) ->
    ?DBG("NewPopAcc:~p~n", [NewPopAcc]),
    {Acc, NewPopAcc}.

% The gather_survivors/3 function accepts the list composed of the alotment tuples and a population normalizer value calculated in
% the competition/3 function, and from those values calculates the actual number of offspring that each agent should produce,
% creating those mutant offspring and accumulating the new generation agent ids. For each Agent_Id the function first calculates
% the noramlized offspring alotment value, to ensure that the final nubmer of agents in the specie is within the popualtion limit
% of that specie. If the offspring alotment value is less than 0, the agent is killed. If the offspring alotment is 1, the parent agent
% is allowed to survive to the next generation, but is not allowed to create any new offspring. If the offspring alotment is greater
% than one, then the Normalized_NextGenAlotment-1 offspring are created from this fit agent, by calling upon the create_MutantAgentCopy/1
% function, which returns the id of the new mutant offspring. Once all the offspring have been created, the function returns to the caller
% a list of ids, composed of the surviving parent agent ids, and their offspring.
gather_survivors([{NextGenAlotment, Fitness, TotNeurons, Agent_Id}|AlotmentsP], Normalizer, Acc) ->
    Normalized_NextGenAlotment = round(NextGenAlotment / Normalizer), % 除以Normalizer，使得总数大约维持在物种的PopulationLimit
    %?DBG("Agent_Id:~p Normalized_NextGenAlotment:~p~n", [Agent_Id, Normalized_NextGenAlotment]),
    SurvivingAgent_Ids = case Normalized_NextGenAlotment >= 1 of
        true ->
            MutantAgent_Ids = case Normalized_NextGenAlotment >= 2 of
                true -> % 新生成除自己外的N-1个变异体
                    [population_monitor:create_MutantAgentCopy(Agent_Id) || _ <- lists:seq(1, Normalized_NextGenAlotment - 1)];
                false -> % 只有1个，那就是它自己
                    []
            end,
            [Agent_Id|MutantAgent_Ids];
        false ->
            ?DBG("Deleting agent:~p~n", [Agent_Id]),
            genotype:delete_Agent(Agent_Id),
            []
    end,
    gather_survivors(AlotmentsP, Normalizer, lists:append(SurvivingAgent_Ids, Acc));
gather_survivors([], _Normalizer, Acc) ->
    %?DBG("New Population:~p, PopSize:~p~n", [Acc, length(Acc)]),
    Acc.

% ProperlySorted_AgentSummaries元素结构：{Fitness, TotNeurons, AgentId}
% TopAgent_Ids是Top 3，根据Top3的agent变异生成子孙（配额平分，每次随机选择），最终的总人口限制在PopulationLimit
top3(ProperlySorted_AgentSummaries, NeuralEnergyCost, PopulationLimit) ->
    TotSurvivors = 3,
    Valid_AgentSummaries = lists:sublist(ProperlySorted_AgentSummaries, TotSurvivors), % 不考虑EFF（效率），直接取fitness前三
    Invalid_AgentSummaries = ProperlySorted_AgentSummaries -- Valid_AgentSummaries,
    {_, _, Valid_AgentIds} = lists:unzip3(Valid_AgentSummaries),
    {_, _, Invalid_AgentIds} = lists:unzip3(Invalid_AgentSummaries),
    [genotype:delete_Agent(Agent_Id) || Agent_Id <- Invalid_AgentIds],
    %?DBG("Valid_AgentSummaries:~p~n", [Valid_AgentSummaries]),
    %?DBG("Invalid_AgentSummaries:~p~n", [Invalid_AgentSummaries]),
    TopAgentSummaries = lists:sublist(Valid_AgentSummaries, 3),
    {_TopFitnessList, _TopTotNs, TopAgent_Ids} = lists:unzip3(TopAgentSummaries),
    %?DBG("NeuralEnergyCost:~p, PopulationLimit:~p~n", [NeuralEnergyCost, PopulationLimit]),
    NewGenAgent_Ids = breed(Valid_AgentIds, PopulationLimit - TotSurvivors, []),
    {NewGenAgent_Ids, TopAgent_Ids}.

% The breed/3 function is part of a very simple selection algorithm, which just selects the top 3 most fit agents, and then uses
% the create_MutantAgentCopy/1 function to create their offspring.
breed(_Valid_AgentIds, 0, Acc) ->
    Acc;
breed(Valid_AgentIds, OffspringIndex, Acc) ->
    Parent_AgentId = lists:nth(rand:uniform(length(Valid_AgentIds)), Valid_AgentIds),
    MutantAgent_Id = population_monitor:create_MutantAgentCopy(Parent_AgentId),
    breed(Valid_AgentIds, OffspringIndex - 1, [MutantAgent_Id|Acc]).

% ProperlySorted_AgentSummaries元素结构：{Fitness, TotNeurons, AgentId}
% 根据所有的agent的适应度计算各自的子孙配额，并将配额拼在一起，然后随机一个数，看随机数落在谁的配额区间内，则返回它的{Fitness, TotNeurons, AgentId}
% 注意，此competition/1（1个参数）为稳态（steady_state）进化算法所用
competition(ProperlySorted_AgentSummaries) ->
    TotEnergy = lists:sum([Fitness || {Fitness, _TotN, _Agent_Id} <- ProperlySorted_AgentSummaries]),
    TotNeurons = lists:sum([TotN || {_Fitness, TotN, _Agent_Id} <- ProperlySorted_AgentSummaries]),
    NeuralEnergyCost = TotEnergy / TotNeurons,
    {AlotmentsP, NextGenTotalSize} = calculate_alotments(ProperlySorted_AgentSummaries, NeuralEnergyCost, [], 0),
    Choice = rand:uniform(),
    {WinnerFitness, WinnerTotN, WinnerAgent_Id} =
        choose_CompetitionWinner(AlotmentsP, NextGenTotalSize, Choice, 0),
    {WinnerFitness, WinnerTotN, WinnerAgent_Id}.

choose_CompetitionWinner([{NextGenAlotment, Fitness, TotN, Agent_Id}|AlotmentsP], NextGenTotalSize, Choice, Range_From) ->
    Range_To = Range_From + NextGenAlotment / NextGenTotalSize,
    % 配额越大，概率越大
    case (Choice >= Range_From) andalso (Choice =< Range_To) of
        true -> % 落在相应的配额区间，返回（找到一个就算）
            {Fitness, TotN, Agent_Id};
        false -> % 未落在相应的配额区间，继续
            choose_CompetitionWinner(AlotmentsP, NextGenTotalSize, Choice, Range_To)
    end;
choose_CompetitionWinner([], _NextGenTotalSize, _Choice, _Range_From) ->
    exit("********ERROR:choose_CompetitionWinner:: reached [] without selecting a winner.").

