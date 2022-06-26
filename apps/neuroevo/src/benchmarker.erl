%% The graphs produced with the data created by the benchmarker process, are plotted by the gnuplot program.
%% 注意三幅图：适应度、NN大小、多样性（包括活着的人口和死亡池中的）
%%
%% 一般常识：
%% 1. 在资源有限的公共scape中，人口少的在初期能够更快地取得较高的适应度（因为资源竞争更少）
%% 2. 一开始就拥有更多传感器的人口能够更快地取得较高的适应度
%% 3. 随着时间流逝，会产生越复杂（NN大小更大）的个体，其探索能力越强、行为更复杂
%% 4. 越复杂的scape，会催生越复杂（NN大小更大）的个体
%% 5. 人口越多，越能探索更多的基因型，能够产生更有效的NN（NN大小越小）
%% 6. 多样性：刚开始要填充死亡池，所以一开始多样性急剧增加，随后保持高多样性的稳定，在活着的人口中，彼此不同的个体将占到75%
%% 7. 人口高死亡率（turnover太快）不利于产生更有效的NN（更有效的行为编码方式），因为没有足够的时间充分验证拓扑结构，导致NN大小以更快的速率增长
%% 8. 人口死亡率是每500次评估统计一次，500次的评估次数是由所有人口共享累积的，所以各物种的死亡率是互补的，总和为500

-module(benchmarker).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

%% Benchmark Options
-define(DIR, "benchmarks/").
-define(INIT_CONSTRAINTS,
        [#constraint{morphology = Morphology,
                     connection_architecture = CA,
                     population_evo_alg_f = generational,
                     neural_pfs = [hebbian]} ||
         Morphology <- [xor_mimic], CA <- [recurrent]]).

-record(graph, {
    morphology,
    avg_neurons = [],
    neurons_std = [],
    avg_fitness = [],
    fitness_std = [],
    max_fitness = [],
    min_fitness = [],
    avg_diversity = [],
    diversity_std = [],
    evaluations = [],
    evaluation_Index = []
}).

-record(avg, {
    avg_neurons = [],
    neurons_std = [],
    avg_fitness = [],
    fitness_std = [],
    max_fitness = [],
    min_fitness = [],
    avg_diversity = [],
    diversity_std = [],
    evaluations = []
}).


% Starts and ends Neural Networks with various preset parameters and options, and polls the logger for information about each run.
start(ExpId) ->
    PMP = #pmp{
        op_mode = gt,
        population_id = test,
        survival_percentage = 0.5,
        specie_size_limit = 20,
        init_specie_size = 20,
        polis_id = mathema,
        generation_limit = inf,
        evaluations_limit = 10000,
        fitness_goal = inf
    }, % 注意，benchmarker设置自己的Population Monitor Parameters
    E = #experiment{
        id = ExpId,
        backup_flag = true,
        pm_parameters = PMP,
        init_constraints = ?INIT_CONSTRAINTS,
        progress_flag = in_progress,
        run_index = 1,
        tot_runs = 20,
        started = {date(), time()},
        interruptions = []
    },
    genotype:write(E),
    register(benchmarker, spawn(benchmarker, prep, [E])).

continue(ExpId) ->
    case genotype:dirty_read({experiment, ExpId}) of
        undefined ->
            ?ERR("Can't continue experiment:~p, it's not present in the database.~n", [ExpId]);
        E ->
            case E#experiment.progress_flag of
                completed ->
                    ?INFO("Experiment:~p already completed:~p~n", [ExpId, E#experiment.trace_acc]);
                in_progress ->
                    Interruptions = E#experiment.interruptions,
                    U_Interruptions = [util:now()|Interruptions], % 记录中断恢复时间戳
                    U_E = E#experiment{interruptions = U_Interruptions},
                    genotype:write(U_E),
                    register(benchmarker, spawn(benchmarker, prep, [U_E]))
            end
    end.

prep(E) ->
    PMP = E#experiment.pm_parameters,
    U_PMP = PMP#pmp{benchmarker_pid = self()},
    Constraints = E#experiment.init_constraints,
    Population_Id = PMP#pmp.population_id,
    population_monitor:prep_PopState(U_PMP, Constraints),
    loop(E#experiment{pm_parameters = U_PMP}, Population_Id).

loop(E, P_Id) ->
    receive
        {P_Id, completed, Trace} -> % 跑完一轮实验
            U_TraceAcc = [Trace|E#experiment.trace_acc],
            U_RunIndex = E#experiment.run_index + 1,
            case U_RunIndex > E#experiment.tot_runs of
                true ->
                    U_E = E#experiment{
                        trace_acc = U_TraceAcc,
                        run_index = U_RunIndex,
                        completed = {date(), time()},
                        progress_flag = completed
                    },
                    genotype:write(U_E),
                    report(U_E#experiment.id, "report");
                false -> % 还没跑完tot_runs轮实验
                    U_E = E#experiment{
                        trace_acc = U_TraceAcc,
                        run_index = U_RunIndex
                    },
                    genotype:write(U_E),
                    PMP = U_E#experiment.pm_parameters,
                    Constraints = U_E#experiment.init_constraints,
                    population_monitor:prep_PopState(PMP, Constraints),
                    loop(U_E, P_Id)
            end;
        terminate ->
            ok
    end.

% xxx_Trace_Acc：收集的原始数据，specie粒度的统计
% xxx_Graphs：morphology粒度的统计
report(Experiment_Id, FileName) ->
    E = genotype:dirty_read({experiment, Experiment_Id}),
    Traces = E#experiment.trace_acc,
    FilePathName = ?DIR ++ FileName ++ "_Trace_Acc",
    {ok, File} = file:open(FilePathName, write),
    lists:foreach(fun(T) -> io:format(File, "~p.~n", [T]) end, Traces),
    file:close(File),
    ?INFO("******** Trace_Acc written to file:~p~n", [FilePathName]),
    Graphs = prepare_Graphs(Traces),
    write_Graphs(Graphs, FileName ++ "_Graphs"),
    Eval_List = [T#trace.tot_evaluations || T <- Traces],
    ?INFO("Tot Evaluations Avg:~p Std:~p~n", [functions:avg(Eval_List), functions:std(Eval_List)]),
    ok.

prepare_Graphs(Traces) ->
    [T|_] = Traces,
    [Stats_List|_] = T#trace.stats,
    % 某一个trace只是某个population_id完成一轮实验的统计，它覆盖多个形态
    Morphologies = [S#stat.morphology || S <- Stats_List], % 从首个Trace的stats首项中即可取得完整的形态列表
    % 遍历每个形态，得到每个形态的统计图
    Morphology_Graphs = [prep_Traces(Traces, Morphology, []) || Morphology <- Morphologies],
    [?INFO("Graph:~p~n", [Graph]) || Graph <- Morphology_Graphs],
    Morphology_Graphs.

% 得到指定形态的统计图
prep_Traces([T|Traces], Morphology, Acc) ->
    % 从T中过滤并累积与指定形态相关的统计数据
    Morphology_Trace = lists:flatten([[S || S <- Stats_List, S#stat.morphology =:= Morphology] || Stats_List <- T#trace.stats]),
    prep_Traces(Traces, Morphology, [Morphology_Trace|Acc]);
prep_Traces([], Morphology, Acc) ->
    Graph = avg_MorphologicalTraces(lists:reverse(Acc), [], [], []),
    Graph#graph{morphology = Morphology}.

% 整理数据，返回统计图（针对某个形态）
avg_MorphologicalTraces([S_List|S_Lists], Acc1, Acc2, Acc3) ->
    % S_List是某个Trace的特定形态的展平的stat列表，S_Lists是各Trace的展平的stat列表的列表
    case S_List of
        [S|STail] ->
            avg_MorphologicalTraces(S_Lists, [STail|Acc1], [S|Acc2], Acc3);
        [] -> % 逻辑出口：所有同位置的元素都已经分层剥离干净，一层层地放在Acc3中了
            Graph = avg_statslists(Acc3, #graph{}),
            Graph
    end;
avg_MorphologicalTraces([], Acc1, Acc2, Acc3) ->
    % 将S_Lists中各Trace的展平的stat列表的同位置（即同一timeslot）元素一层一层地剥离，将剥离后的一层放到Acc3中
    avg_MorphologicalTraces(lists:reverse(Acc1), [], [], [lists:reverse(Acc2)|Acc3]).

% 返回统计图：某形态在不同timeslot的平均值
avg_statslists([S_List|S_Lists], Graph) ->
    % S_List为一层
    Avg = avg_stats(S_List, #avg{}),
    U_Graph = Graph#graph{
        avg_neurons = [Avg#avg.avg_neurons|Graph#graph.avg_neurons],
        neurons_std = [Avg#avg.neurons_std|Graph#graph.neurons_std],
        avg_fitness = [Avg#avg.avg_fitness|Graph#graph.avg_fitness],
        fitness_std = [Avg#avg.fitness_std|Graph#graph.fitness_std],
        max_fitness = [Avg#avg.max_fitness|Graph#graph.max_fitness],
        min_fitness = [Avg#avg.min_fitness|Graph#graph.min_fitness],
        evaluations = [Avg#avg.evaluations|Graph#graph.evaluations],
        avg_diversity = [Avg#avg.avg_diversity|Graph#graph.avg_diversity],
        diversity_std = [Avg#avg.diversity_std|Graph#graph.diversity_std]
    },
    avg_statslists(S_Lists, U_Graph);
avg_statslists([], Graph) ->
    Graph#graph{
        avg_neurons = lists:reverse(Graph#graph.avg_neurons),
        neurons_std = lists:reverse(Graph#graph.neurons_std),
        avg_fitness = lists:reverse(Graph#graph.avg_fitness),
        fitness_std = lists:reverse(Graph#graph.fitness_std),
        max_fitness = lists:reverse(Graph#graph.max_fitness),
        min_fitness = lists:reverse(Graph#graph.min_fitness),
        evaluations = lists:reverse(Graph#graph.evaluations),
        avg_diversity = lists:reverse(Graph#graph.avg_diversity),
        diversity_std = lists:reverse(Graph#graph.diversity_std)
    }.

% 对一层数据进行统计计算：对stat数据结构中各字段分别统计计算
% 某形态在同一个timeslot内的不同物种的数据平均（参看population_monitor的gather_STATS函数）
avg_stats([S|STail], Avg) ->
    U_Avg = Avg#avg{
        avg_neurons = [S#stat.avg_neurons|Avg#avg.avg_neurons],
        avg_fitness = [S#stat.avg_fitness|Avg#avg.avg_fitness],
        max_fitness = [S#stat.max_fitness|Avg#avg.max_fitness],
        min_fitness = [S#stat.min_fitness|Avg#avg.min_fitness],
        evaluations = [S#stat.evaluations|Avg#avg.evaluations],
        avg_diversity = [S#stat.avg_diversity|Avg#avg.avg_diversity]
    },
    avg_stats(STail, U_Avg);
avg_stats([], Avg) ->
    Avg#avg{
        avg_neurons = functions:avg(Avg#avg.avg_neurons),
        neurons_std = functions:std(Avg#avg.avg_neurons),
        avg_fitness = functions:avg(Avg#avg.avg_fitness),
        fitness_std = functions:std(Avg#avg.avg_fitness),
        max_fitness = lists:max(Avg#avg.max_fitness),
        min_fitness = lists:min(Avg#avg.min_fitness),
        evaluations = functions:avg(Avg#avg.evaluations),
        avg_diversity = functions:avg(Avg#avg.avg_diversity),
        diversity_std = functions:std(Avg#avg.avg_diversity)
    }.

% 将统计图数据写入图文件
write_Graphs([G|Graphs], Graph_Postfix) ->
    Morphology = G#graph.morphology,
    U_G = G#graph{evaluation_Index = [500 * Index || Index <- lists:seq(1, length(G#graph.avg_fitness))]}, % 构建timeslots坐标
    {ok, File} = file:open(?DIR ++ "graph_" ++ atom_to_list(Morphology) ++ "_" ++ Graph_Postfix, write),
    io:format(File, "#Avg Fitness Vs Evaluations, Morphology:~p~n", [Morphology]),
    lists:foreach(fun({X, Y, Std}) -> io:format(File, "~p ~p ~p~n", [X, Y, Std]) end,
                  lists:zip3(U_G#graph.evaluation_Index, U_G#graph.avg_fitness, U_G#graph.fitness_std)),
    io:format(File, "~n~n#Avg Neurons Vs Evaluations, Morphology:~p~n", [Morphology]),
    lists:foreach(fun({X, Y, Std}) -> io:format(File, "~p ~p ~p~n", [X, Y, Std]) end,
                  lists:zip3(U_G#graph.evaluation_Index, U_G#graph.avg_neurons, U_G#graph.neurons_std)),
    io:format(File, "~n~n#Avg Diversity Vs Evaluations, Morphology:~p~n", [Morphology]),
    lists:foreach(fun({X, Y, Std}) -> io:format(File, "~p ~p ~p~n", [X, Y, Std]) end,
                  lists:zip3(U_G#graph.evaluation_Index, U_G#graph.avg_diversity, U_G#graph.diversity_std)),
    io:format(File, "~n~n#Max Fitness Vs Evaluations, Morphology:~p~n", [Morphology]),
    lists:foreach(fun({X, Y}) -> io:format(File, "~p ~p~n", [X, Y]) end,
                  lists:zip(U_G#graph.evaluation_Index, U_G#graph.max_fitness)),
    io:format(File, "~n~n#Min Fitness Vs Evaluations, Morphology:~p~n", [Morphology]),
    lists:foreach(fun({X, Y}) -> io:format(File, "~p ~p~n", [X, Y]) end,
                  lists:zip(U_G#graph.evaluation_Index, U_G#graph.min_fitness)),
    io:format(File, "~n~n#Specie-Population Turnover Vs Evaluations, Morphology:~p~n", [Morphology]),
    lists:foreach(fun({X, Y}) -> io:format(File, "~p ~p~n", [X, Y]) end,
                  lists:zip(U_G#graph.evaluation_Index, U_G#graph.evaluations)),
    file:close(File),
    write_Graphs(Graphs, Graph_Postfix);
write_Graphs([], _Graph_Postfix) ->
    ok.

unconsult(List) ->
    {ok, File} = file:open(?DIR ++ "alife_benchmark", write),
    lists:foreach(fun(X) -> io:format(File, "~p~n", [X]) end, List),
    file:close(File).

% 求每一层的平均值
gen_avg_plot(Lists) ->
    gen_avg_plot(Lists, [], [], []).

gen_avg_plot([List|Lists], Acc1, Acc2, Acc3) ->
    % 同avg_MorphologicalTraces的处理方式类似
    case List of
        [Val|Rem] ->
            gen_avg_plot(Lists, [Rem|Acc1], [Val|Acc2], Acc3);
        [] ->
            print_plot(500, lists:reverse(Acc3))
    end;
gen_avg_plot([], Acc1, Acc2, Acc3) ->
    gen_avg_plot(Acc1, [], [], [functions:avg(Acc2)|Acc3]).

gen_max_plot(Lists) ->
    gen_max_plot(Lists, []).

gen_max_plot([L|Lists], Acc) ->
    gen_max_plot(Lists, [lists:max(L)|Acc]);
gen_max_plot([], Acc) ->
    print_plot(0, lists:reverse(Acc)).

print_plot(Index, [Val|List]) ->
    io:format("~p  ~p~n", [Index, Val]),
    print_plot(Index + 500, List);
print_plot(_Index, []) ->
    void.

% TraceFileName：report/2里写入的trace文件
trace2graph(TraceFileName) ->
    {ok, Traces} = file:consult(TraceFileName),
    io:format("Traces: ~p~n", [Traces]),
    Graphs = prepare_Graphs(Traces),
    write_Graphs(Graphs, TraceFileName ++ "_Graph").

