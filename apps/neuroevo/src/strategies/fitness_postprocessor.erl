%% 用于适应度的后处理调整

-module(fitness_postprocessor).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

-define(EFF, 0.1). % Efficiency

none(Agent_Summaries) ->
    lists:reverse(lists:sort(Agent_Summaries)).

% 与尺寸成比例，考虑尺寸（神经元数目）的因素，Fitness除以它做调整，这样会鼓励尺寸越小的个体
% 返回：按调整后的适应度降序排列的AgentSummaries
size_proportional(Agent_Summaries) ->
    AdjustedFitnessPairs = [{Fitness / math:pow(TotN, ?EFF), {Fitness, TotN, Agent_Id}} || {Fitness, TotN, Agent_Id} <- Agent_Summaries],
    SDX = lists:reverse(lists:sort(AdjustedFitnessPairs)),
    ProperlySorted_AgentSummaries = [Val || {_, Val} <- SDX],
    ProperlySorted_AgentSummaries.

