%% 模因方法：
%% 1. exoself：调权重，在NN完成一组权重的评估后，< max_attempts时，选择部分神经元扰动权重
%% 2. population_monitor：调拓扑（也有权重变异MO），按MO的slice占比概率选MO，由tot_topological_mutations决定变异次数，若选中的MO为权重变异，则随机选1个神经元扰动权重
%% 3. 当max_attempts设为0时，则模因方法退化为基因方法

-module(population_monitor).
-behaviour(gen_server).

%% 启停API
-export([start_link/2, start_link/1, start/2, start/1, stop/1, init/2]).

%% 功能API
-export([extract_AgentIds/2,
         prep_PopState/2,
         init_population/2,
         create_specie/3,
         continue/0,
         continue/1,
         create_MutantAgentCopy/1,
         delete_population/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

%%====================================================================
%% Population Monitor Options & Parameters
%%====================================================================
% 每一形态、每一连接架构，采用稳态人口进化算法
-define(INIT_CONSTRAINTS(Morphologies),
        [#constraint{morphology = Morphology,
                     connection_architecture = CA,
                     population_evo_alg_f = steady_state,
                     agent_encoding_types = [neural],
                     substrate_plasticities = [iterative],
                     substrate_linkforms = [l2l_feedforward]} ||
         Morphology <- Morphologies, CA <- [recurrent]]).

-define(SPECIE_ACTIVE_SIZE_LIMIT, 10).

-record(state, {
    op_mode = gt, % 运作模式：standard，throughput，gt（genetic tuning）
    population_id = test,
    activeAgent_IdPs = [], % {Agent_Id, Agent_PId}的列表
    agent_ids = [],
    tot_agents,
    agents_left,
    op_tag, % 运作标签：continue，pause，done
    agent_summaries = [],
    pop_gen = 0, % 人口代数
    eval_acc = 0, % 评估次数累积（每一次评估对应一组权重，该变量意味着试了多少组权重），所有人口都共享累积它
    cycle_acc = 0, % 训练次数累积（每一次训练对应一次sense-think-act过程），所有人口都共享累积它
    time_acc = 0, % 训练时间累积，所有人口都共享累积它
    tot_evaluations = 0, % 评估次数累积，和eval_acc的区别：tot_evaluations在超过step_size后不清零，eval_acc、cycle_acc、time_acc则清零
    step_size, % 每多少步（多少次评估），计算一次统计数据
    goal_status,
    evolutionary_algorithm, % 人口进化算法：generational，steady_state
    fitness_postprocessor, % 适应度的后处理调整函数
    selection_algorithm, % 淘汰选择算法函数
    best_fitness,
    survival_percentage = 0.5, % 选择阶段，允许多少比例的人口存活下去
    specie_size_limit = 10, % 每一个物种内的原型数量限制
    init_specie_size = ?SPECIE_ACTIVE_SIZE_LIMIT, % 初始的物种内的原型数量
    polis_id = mathema, % 城邦id
    generation_limit = 100, % 针对pop_gen
    evaluations_limit = inf, % 针对tot_evaluations
    fitness_goal = inf,
    benchmarker_pid,
    goal_reached = false
}).

%%====================================================================
%% 启停API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(UserId, Start_Parameters) ->
    gen_server:start_link(?MODULE, {UserId, Start_Parameters}, []).

start_link(UserId) ->
    gen_server:start_link(?MODULE, {UserId, []}, []).

start(UserId, Start_Parameters) ->
    gen_server:start(?MODULE, {UserId, Start_Parameters}, []).

% Starts the population monitor through init_population/2 with a set of default parameters specified by the macros of this module.
start(UserId) ->
    {ok, Cfg} = application:get_env(neuroevo, population_monitor),
    {_, Morphologies} = lists:keyfind(morphologies, 1, Cfg),
    init_population(#state{population_id = UserId}, ?INIT_CONSTRAINTS(Morphologies)).

stop(UserId) ->
    PopMonName = <<"population_monitor_", (integer_to_binary(UserId))/binary>>,
    case gproc:where({n, l, PopMonName}) of
        undefined ->
            ?INFO("PopMon for user=~p cannot be stopped, it is not online.~n", [UserId]);
        PopMon_PId ->
            gproc:unreg_other({n, l, PopMonName}, PopMon_PId),
            gen_server:cast(PopMon_PId, {stop, normal})
    end.

init(Pid, InitState) ->
    gen_server:cast(Pid, {init, InitState}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
% In init/1 the population_monitor process registers itself with the node under the name monitor, and sets all the needed parameters within
% its #state record. The function first extracts all the Agent_Ids that belong to the population using the extract_AgentIds/2 function.
% Each agent is then spawned/activated, converted from genotype to phenotype in the summon_agents/2 function. The summon_agents/2 function
% summons the agents and returns to the caller a list of tuples with the following format: [{Agent_Id, Agent_PId}...]. Once the state record's
% parameters have been set, the function drops into the main gen_server loop.
init({UserId, S}) ->
    process_flag(trap_exit, true),
    PopMonName = <<"population_monitor_", (integer_to_binary(UserId))/binary>>,
    gproc:reg({n, l, PopMonName}, self()),
    Population_Id = S#state.population_id,
    OpMode = S#state.op_mode,
    ?INFO("******** Population monitor started with parameters:~p~n", [S]),
    Agent_Ids = extract_AgentIds(Population_Id, all),
    ActiveAgent_IdPs = summon_agents(OpMode, Agent_Ids, UserId),
    P = genotype:dirty_read({population, Population_Id}),
    [put({evaluations, Specie_Id}, 0) || Specie_Id <- P#population.specie_ids],
    T = P#population.trace,
    TotEvaluations = T#trace.tot_evaluations,
    ?INFO("Initial Tot Evaluations:~p~n", [TotEvaluations]),
    State = S#state{
        population_id = Population_Id,
        activeAgent_IdPs = ActiveAgent_IdPs,
        tot_agents = length(Agent_Ids),
        agents_left = length(Agent_Ids),
        op_tag = continue,
        evolutionary_algorithm = P#population.evo_alg_f,
        fitness_postprocessor = P#population.fitness_postprocessor_f,
        selection_algorithm = P#population.selection_f,
        best_fitness = 0,
        step_size = T#trace.step_size,
        tot_evaluations = TotEvaluations
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

% If the population_monitor process receives a {stop, normal} call, it checks if there are any still active agents. If there are any,
% it terminates them, and then itself terminates.
handle_call({stop, normal}, _From, S) ->
    ActiveAgent_IdPs = S#state.activeAgent_IdPs,
    [Agent_PId ! {self(), terminate} || {_Agent_Id, Agent_PId} <- ActiveAgent_IdPs],
    {stop, normal, S};

handle_call({stop, shutdown}, _From, State) ->
    {stop, shutdown, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({init, InitState}, _State) ->
    {noreply, InitState};

% This clause accepts the cast signals sent by the agents which terminate after finishing with their evaluations. The clause specialises in the
% "competition" selection algorithm, which is a generational selection algorithm. As a generation selection algorithm, it waits until the entire
% population has finished being evaluated, and only then selects the fit from the unfit, and creates the updated population of the next generation.
% The OpTag can be set from the outside to shutdown the population_monitor by setting it to done. Once an ending condition is reached, either through
% a generation limit, an evaluations limit, or fitness goal, the population_monitor exits normally. If the ending condition is not reached,
% the population_monitor spawns the new generation of agents and awaits again for all the agents in the population to complete their evaluations.
% If the OpTag is set to pause, it does not generate a new population, and instead goes into a waiting mode, and awaits to be restarted or terminated.
handle_cast({Agent_Id, terminated, Fitness}, #state{evolutionary_algorithm = generational, % 世代（等所有人口均完成评估才生成下一代）
                                                    population_id = Population_Id,
                                                    op_tag = OpTag,
                                                    op_mode = OpMode,
                                                    agents_left = AgentsLeft,
                                                    specie_size_limit = SpecieSizeLimit,
                                                    fitness_postprocessor = FitnessPostprocessor,
                                                    selection_algorithm = SelectionAlgorithm,
                                                    pop_gen = PopGen,
                                                    generation_limit = Generation_Limit,
                                                    evaluations_limit = Evaluation_Limit,
                                                    fitness_goal = Fitness_Goal,
                                                    tot_evaluations = TotEvaluations,
                                                    goal_reached = GoalReached,
                                                    activeAgent_IdPs = ActiveAgent_IdPs} = S) ->
    case AgentsLeft =< 1 of
        true -> % 本来<=1个，收到这个消息就<=0个
            mutate_population(Population_Id, SpecieSizeLimit, FitnessPostprocessor, SelectionAlgorithm),
            U_PopGen = PopGen + 1,
            ?DBG("Population Generation:~p.~n", [U_PopGen]),
            case OpTag of
                continue ->
                    Specie_Ids = (genotype:dirty_read({population, Population_Id}))#population.specie_ids,
                    SpecieFitList = [(genotype:dirty_read({specie, Specie_Id}))#specie.fitness || Specie_Id <- Specie_Ids],
                    BestFitness = lists:max([MaxFitness || {_, _, MaxFitness, _} <- SpecieFitList]),
                    case (U_PopGen >= Generation_Limit) orelse (TotEvaluations >= Evaluation_Limit) orelse
                             (BestFitness >= Fitness_Goal) orelse GoalReached of
                        true -> % ending condition reached
                            Agent_Ids = extract_AgentIds(Population_Id, all),
                            TotAgents = length(Agent_Ids),
                            U_S = S#state{agent_ids = Agent_Ids, tot_agents = TotAgents, agents_left = TotAgents, pop_gen = U_PopGen},
                            {stop, normal, U_S};
                        false -> % in progress
                            Agent_Ids = extract_AgentIds(Population_Id, all),
                            U_ActiveAgent_IdPs = summon_agents(OpMode, Agent_Ids, Population_Id),
                            TotAgents = length(Agent_Ids),
                            U_S = S#state{activeAgent_IdPs = U_ActiveAgent_IdPs, tot_agents = TotAgents, agents_left = TotAgents, pop_gen = U_PopGen},
                            {noreply, U_S}
                    end;
                done ->
                    ?INFO("Shutting down Population Monitor.~n"),
                    U_S = S#state{agents_left = 0, pop_gen = U_PopGen},
                    {stop, normal, U_S};
                pause ->
                    ?INFO("Population Monitor has paused.~n"),
                    U_S = S#state{agents_left = 0, pop_gen = U_PopGen},
                    {noreply, U_S}
            end;
        false ->
            ?INFO("Agents Left:~p~n", [AgentsLeft - 1]),
            U_ActiveAgent_Ids = lists:keydelete(Agent_Id, 1, ActiveAgent_IdPs),
            U_S = S#state{activeAgent_IdPs = U_ActiveAgent_Ids, agents_left = AgentsLeft - 1},
            {noreply, U_S}
    end;

handle_cast({Agent_Id, terminated, Fitness}, #state{evolutionary_algorithm = steady_state, % 稳态（有人口损失时随时从原型池中挑选原型补充实例）
                                                    population_id = Population_Id,
                                                    specie_size_limit = SpecieSizeLimit,
                                                    survival_percentage = SurvivalPercentage,
                                                    fitness_postprocessor = FitnessPostprocessor,
                                                    selection_algorithm = SelectionAlgorithm,
                                                    evaluations_limit = Evaluation_Limit,
                                                    fitness_goal = Fitness_Goal,
                                                    best_fitness = BestFitness,
                                                    tot_evaluations = TotEvaluations,
                                                    goal_reached = GoalReached,
                                                    activeAgent_IdPs = ActiveAgent_IdPs} = S) ->
    NewBestFitness = case Fitness > BestFitness of
        true -> Fitness;
        false -> BestFitness
    end,
    case (TotEvaluations >= Evaluation_Limit) orelse (NewBestFitness >= Fitness_Goal) orelse GoalReached of
        true ->
            case lists:keydelete(Agent_Id, 1, ActiveAgent_IdPs) of
                [] ->
                    U_S = S#state{activeAgent_IdPs = []},
                    {stop, normal, U_S};
                U_ActiveAgent_IdPs -> % 如果还有active agent，则不stop
                    U_S = S#state{activeAgent_IdPs = U_ActiveAgent_IdPs},
                    {noreply, U_S}
            end;
        false ->
            A = genotype:dirty_read({agent, Agent_Id}),
            Morphology = (A#agent.constraint)#constraint.morphology,
            ?INFO("Agent_Id:~p of morphology:~p with fitness:~p terminated.~n", [Agent_Id, Morphology, Fitness]),
            Specie_Id = A#agent.specie_id,
            Specie = genotype:dirty_read({specie, Specie_Id}),
            Old_DeadPool_AgentSummaries = Specie#specie.dead_pool,
            Old_Agent_Ids = Specie#specie.agent_ids,
            [AgentSummary] = construct_AgentSummaries([Agent_Id], []),
            DeadPool_AgentSummaries =
                case lists:keyfind(Agent_Id, 3, Old_DeadPool_AgentSummaries) of
                    false -> [AgentSummary | Old_DeadPool_AgentSummaries];
                    {OldFitness, _, _} ->
                        {NewFitness, _, _} = AgentSummary,
                        case NewFitness > OldFitness of
                            true -> lists:keyreplace(Agent_Id, 3, Old_DeadPool_AgentSummaries, AgentSummary);
                            false -> Old_DeadPool_AgentSummaries
                        end
                end,
            ProperlySorted_AgentSummaries = fitness_postprocessor:FitnessPostprocessor(DeadPool_AgentSummaries),
            Valid_AgentSummaries = case length(ProperlySorted_AgentSummaries) >= SpecieSizeLimit of
                true -> % 超过物种内原型数量限制，删除最不优秀的那一个
                    [{InvalidFitness, InvalidTotN, InvalidAgent_Id} | Remaining_AgentSummaries] = lists:reverse(ProperlySorted_AgentSummaries),
                    ?DBG("Information theoretic Death:~p::~p~n",[InvalidAgent_Id, {InvalidFitness, InvalidTotN, InvalidAgent_Id}]),
                    genotype:delete_Agent(InvalidAgent_Id, safe),
                    lists:reverse(Remaining_AgentSummaries);
                false ->
                    ProperlySorted_AgentSummaries
            end,
            %?INFO("Valid_AgentSummaries:~p~n", [Valid_AgentSummaries]),
            % 从有效的原型中挑选一个
            {WinnerFitness, WinnerProfile, WinnerAgent_Id} = selection_algorithm:SelectionAlgorithm(Valid_AgentSummaries), % 1个参数的
            ActiveAgent_IdP = case rand:uniform() < 0.1 of
                true -> % 10%的概率，直接使用选中的原型
                    U_DeadPool_AgentSummaries = lists:delete({WinnerFitness, WinnerProfile, WinnerAgent_Id}, Valid_AgentSummaries),
                    WinnerAgent_PId = exoself:start(WinnerAgent_Id, self(), Population_Id),
                    {WinnerAgent_Id, WinnerAgent_PId};
                false -> % 90%的概率，创建变异体
                    U_DeadPool_AgentSummaries = Valid_AgentSummaries,
                    CloneAgent_Id = create_MutantAgentCopy(WinnerAgent_Id, safe),
                    CloneAgent_PId = exoself:start(CloneAgent_Id, self(), Population_Id),
                    {CloneAgent_Id, CloneAgent_PId}
            end,
            Top_AgentSummaries = lists:sublist(U_DeadPool_AgentSummaries, round(SpecieSizeLimit * SurvivalPercentage)),
            {_, _, TopAgent_Ids} = lists:unzip3(lists:sublist(Top_AgentSummaries, 3)),
            ?DBG("TopAgent_Ids:~p~n", [TopAgent_Ids]),
            USpecie = genotype:dirty_read({specie, Specie_Id}),
            genotype:write(USpecie#specie{dead_pool = U_DeadPool_AgentSummaries, champion_ids = TopAgent_Ids}),
            ActiveAgent_IdPs = S#state.activeAgent_IdPs,
            U_ActiveAgent_IdPs = [ActiveAgent_IdP | lists:keydelete(Agent_Id, 1, ActiveAgent_IdPs)],
            U_S = S#state{
                activeAgent_IdPs = U_ActiveAgent_IdPs,
                best_fitness = NewBestFitness
            },
            {noreply, U_S}
    end;

handle_cast({Agent_Id, stuck}, #state{evolutionary_algorithm = steady_state, % 稳态（有人口损失时随时从原型池中挑选原型补充实例）
                                      population_id = Population_Id,
                                      activeAgent_IdPs = ActiveAgent_IdPs} = S) ->
    NewAgent_PId = exoself:start(Agent_Id, self(), Population_Id),
    U_ActiveAgent_IdPs = [{Agent_Id, NewAgent_PId} | lists:keydelete(Agent_Id, 1, ActiveAgent_IdPs)],
    U_S = S#state{
        activeAgent_IdPs = U_ActiveAgent_IdPs
    },
    ?INFO("Agent_Id:~p stuck, already terminated, and restarted.~n", [Agent_Id]),
    {noreply, U_S};

% The population_monitor process accepts a pause command cast, which if it recieves, it then goes into pause mode after all the agents have completed
% with their evaluations. The process can only go into pause mode if it is currently in the continue mode (its op_tag is set to continue).
handle_cast({op_tag, pause}, S) when S#state.op_tag =:= continue ->
    U_S = S#state{op_tag = pause},
    {noreply, U_S};

% The population_monitor process can accept a continue command if its current op_tag is set to pause. When it receives a continue command, it summons
% all the agents in the population, and continues with its neuroevolution synchronization duties.
handle_cast({op_tag, continue}, S) when S#state.op_tag =:= pause ->
    Population_Id = S#state.population_id,
    OpMode = S#state.op_mode,
    Agent_Ids = extract_AgentIds(Population_Id, all),
    U_ActiveAgent_IdPs = summon_agents(OpMode, Agent_Ids, Population_Id),
    TotAgents = length(Agent_Ids),
    U_S = S#state{activeAgent_IdPs = U_ActiveAgent_IdPs, tot_agents = TotAgents, agents_left = TotAgents, op_tag = continue},
    {noreply, U_S};

handle_cast({From, evaluations, Specie_Id, AgentEvalAcc0, AgentCycleAcc, AgentTimeAcc}, S) ->
    AgentEvalAcc = case S#state.goal_reached of
        true -> % 已达目标，eval不再累计
            0;
        false ->
            AgentEvalAcc0
    end,
    Eval_Acc = S#state.eval_acc,
    U_EvalAcc = S#state.eval_acc + AgentEvalAcc,
    U_CycleAcc = S#state.cycle_acc + AgentCycleAcc,
    U_TimeAcc = S#state.time_acc + AgentTimeAcc,
    U_TotEvaluations = S#state.tot_evaluations + AgentEvalAcc,
    SEval_Acc = get({evaluations, Specie_Id}), % 特定某物种的eval累计
    put({evaluations, Specie_Id}, SEval_Acc + AgentEvalAcc),
    case Eval_Acc rem 50 of
        0 ->
            ?INFO("Evaluations/Step:~p~n", [Eval_Acc]);
        _ ->
            done
    end,
    U_S = case U_EvalAcc >= S#state.step_size of
        true -> % eval累计超过step_size，触发一次统计（然后eval_acc重置为零）
            gather_STATS(S#state.population_id, U_EvalAcc),
            Population_Id = S#state.population_id,
            P = genotype:dirty_read({population, Population_Id}),
            T = P#population.trace,
            TotEvaluations = T#trace.tot_evaluations, % trace结构里的tot_evaluations是在前面gather_STATS中更新的
            ?INFO("Tot Evaluations:~p~n", [TotEvaluations]),
            S#state{eval_acc = 0, cycle_acc = 0, time_acc = 0, tot_evaluations = U_TotEvaluations};
        false ->
            S#state{eval_acc = U_EvalAcc, cycle_acc = U_CycleAcc, time_acc = U_TimeAcc, tot_evaluations = U_TotEvaluations}
    end,
    {noreply, U_S};

handle_cast({_From, goal_reached}, S) ->
    U_S = S#state{goal_reached = true},
    {noreply, U_S};

handle_cast({_From, print_TRACE}, S) ->
    Population_Id = S#state.population_id,
    P = genotype:dirty_read({population, Population_Id}),
    ?INFO("******** TRACE ********:~n~p~n", [P#population.trace]),
    {noreply, S};

handle_cast({stop, normal}, State) ->
    {stop, normal, State};

handle_cast({stop, shutdown}, State) ->
    {stop, shutdown, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

% When the population_monitor process terminates, it states so, notifies with what op_tag and op_mode it terminated, all the stats gathered,
% and then shuts down.
terminate(Reason, S) ->
    case S of
        [] ->
            ?INFO("******** Population_Monitor shut down with Reason:~p, with State: []~n", [Reason]);
        _ ->
            ActiveAgent_IdPs = S#state.activeAgent_IdPs,
            [Agent_PId ! {self(), terminate} || {_Agent_Id, Agent_PId} <- ActiveAgent_IdPs],
            OpMode = S#state.op_mode,
            OpTag = S#state.op_tag,
            TotEvaluations = S#state.tot_evaluations,
            Population_Id = S#state.population_id,
            case TotEvaluations < 500 of
                true -> % So that there is at least one stat in the stats list.
                    gather_STATS(Population_Id, 0);
                false ->
                    ok
            end,
            P = genotype:dirty_read({population, Population_Id}),
            T = P#population.trace,
            U_T = T#trace{tot_evaluations = TotEvaluations},
            U_P = P#population{trace = U_T},
            genotype:write(U_P),
            ?INFO("******** TRACE START >>>>>>>>~n"),
            ?INFO("~p~n", [U_T]),
            ?INFO("******** TRACE END <<<<<<<<~n"),
            ?INFO("******** Population_Monitor:~p shut down with Reason:~p, OpTag:~p, while in OpMode:~p~n", [Population_Id, Reason, OpTag, OpMode]),
            ?INFO("******** Tot Agents:~p, Population Generation:~p, Tot_Evals:~p~n", [S#state.tot_agents, S#state.pop_gen, S#state.tot_evaluations]),
            case S#state.benchmarker_pid of
                undefined ->
                    ok;
                PId ->
                    PId ! {S#state.population_id, completed, U_T} % 通知benchmarker跑完一轮实验
            end
    end.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

% The extract_AgentIds/2 function accepts the Population_Id and a parameter which specifies what type of agents (all agent ids, or just those of
% the champions) to extract from the population, after which it extracts those agents. Depending on the AgentType parameter, the function either
% calls extract_ChampionAgentIds/2 or extract_AllAgentIds/2, which return the list of agent ids to the caller.
extract_AgentIds(Population_Id, AgentType) ->
    P = genotype:dirty_read({population, Population_Id}),
    Specie_Ids = P#population.specie_ids,
    ?DBG("Specie_Ids:~p~n", [Specie_Ids]),
    case AgentType of
        champions ->
            extract_ChampionAgentIds(Specie_Ids, []);
        all ->
            extract_AllAgentIds(Specie_Ids, [])
    end.

% extract_ChampionAgentIds/2 accumulates the ids of champion agents from every specie in the Specie_Ids list, and then returns that list to the caller.
extract_ChampionAgentIds([Specie_Id|Specie_Ids], Acc) ->
    S = genotype:dirty_read({specie, Specie_Id}),
    ChampionAgent_Ids = S#specie.champion_ids,
    extract_ChampionAgentIds(Specie_Ids, lists:append(ChampionAgent_Ids, Acc));
extract_ChampionAgentIds([], Acc) ->
    Acc.

% extract_AllAgentIds/2 accumulates and returns to the caller an id list of all the agents belonging to the species in the Specie_Ids list.
extract_AllAgentIds([Specie_Id|Specie_Ids], Acc) ->
    S = genotype:dirty_read({specie, Specie_Id}),
    AllAgent_Ids = S#specie.agent_ids,
    ActiveSize = length(AllAgent_Ids),
    case ActiveSize > ?SPECIE_ACTIVE_SIZE_LIMIT of
        true ->
            {KeepAgent_Ids, DelAgent_Ids} = lists:split(?SPECIE_ACTIVE_SIZE_LIMIT, AllAgent_Ids),
            genotype:write(S#specie{agent_ids = KeepAgent_Ids}),
            [case lists:keymember(Agent_Id, 3, S#specie.dead_pool) of
                 true -> void;
                 false -> genotype:delete_Agent(Agent_Id, safe)
             end || Agent_Id <- DelAgent_Ids],
            extract_AllAgentIds(Specie_Ids, lists:append(KeepAgent_Ids, Acc));
        false ->
            extract_AllAgentIds(Specie_Ids, lists:append(AllAgent_Ids, Acc))
    end;
extract_AllAgentIds([], Acc) ->
    Acc.

% The summon_agents/2 and summon_agents/3 spawns all the agents in the Agent_ids list, and returns to the caller a list of tuples as follows:
% [{Agent_Id,Agent_PId}...].
summon_agents(OpMode, Agent_Ids, Population_Id) ->
    summon_agents(OpMode, Agent_Ids, Population_Id, []).

summon_agents(OpMode, [Agent_Id|Agent_Ids], Population_Id, Acc) ->
    ?DBG("Agent_Id:~p~n", [Agent_Id]),
    Agent_PId = exoself:start(Agent_Id, self(), Population_Id),
    summon_agents(OpMode, Agent_Ids, Population_Id, [{Agent_Id, Agent_PId}|Acc]);
summon_agents(_OpMode, [], _Population_Id, Acc) ->
    Acc.

% 注意区别：start/0里调用init_population时传入的State是默认构造的#state{}，传入的Specie_Constraints是依据配置文件中的morphologies构造而成
prep_PopState(PMP, Specie_Constraints) ->
    S = #state{
        op_mode = PMP#pmp.op_mode,
        population_id = PMP#pmp.population_id,
        survival_percentage = PMP#pmp.survival_percentage,
        specie_size_limit = PMP#pmp.specie_size_limit,
        init_specie_size = PMP#pmp.init_specie_size,
        polis_id = PMP#pmp.polis_id,
        generation_limit = PMP#pmp.generation_limit,
        evaluations_limit = PMP#pmp.evaluations_limit,
        fitness_goal = PMP#pmp.fitness_goal,
        benchmarker_pid = PMP#pmp.benchmarker_pid
    },
    init_population(S, Specie_Constraints).

% The function init_population/2 creates a new population with the id Population_Id, composed of length(Specie_Constraints) species, where each specie
% uses the particular specie constraint specified within the Specie_Constraints list. The function first checks if a population with the noted Population_Id
% already exists, if a population does exist, then the function first deletes it, and then creates a fresh one. Since the ids are usually generated with
% the genotype:create_UniqueId/0, the only way an already existing Population_Id is dropped into the function as a parameter is if it is intended,
% usually when runing tests, with the Population_Id = test.
init_population(Init_State, Specie_Constraints) ->
    rand:seed(exs64, util:now()),
    Population_Id = Init_State#state.population_id,
    OpMode = Init_State#state.op_mode,
    %F = fun() ->
    %    case genotype:read({population, Population_Id}) of
    %        undefined -> void;
    %        _ -> delete_population(Population_Id)
    %    end,
    %    create_Population(Population_Id, Init_State#state.init_specie_size, Specie_Constraints)
    %end,
    F = fun() ->
        case genotype:read({population, Population_Id}) of
            undefined -> create_Population(Population_Id, Init_State#state.init_specie_size, Specie_Constraints);
            _ -> void
        end
    end,
    Result = mnesia:transaction(F),
    case Result of
        {atomic, _} ->
            % 开启gen_server进程
            population_monitor:start(Population_Id, Init_State);
        Error ->
            ?ERR("******** ERROR in PopulationMonitor:~p~n", [Error])
    end.

% The create_Population/3 generates length(Specie_Constraints) number of specie, each composed of ?INIT_SPECIE_SIZE number of agents.
% The function uses the create_specie/4 to generate the species.
create_Population(Population_Id, SpecieSize, Specie_Constraints) ->
    Specie_Ids = [create_specie(Population_Id, SpecCon, origin, SpecieSize) || SpecCon <- Specie_Constraints],
    [C|_] = Specie_Constraints, % 为什么取第一个，因为它们以下的三个函数都一样
    Population = #population{
        id = Population_Id,
        specie_ids = Specie_Ids,
        evo_alg_f = C#constraint.population_evo_alg_f,
        fitness_postprocessor_f = C#constraint.population_fitness_postprocessor_f,
        selection_f = C#constraint.population_selection_f
    },
    genotype:write(Population).

% The create_specie/3 and create_specie/4 functions are simplified versions which use default parameters to call the create_specie/6 function.
% 未指定SpecieSize，则创建一个空的物种
create_specie(Population_Id, SpeCon, Fingerprint) ->
    Specie_Id = genotype:generate_UniqueId(),
    create_specie(Population_Id, Specie_Id, 0, [], SpeCon, Fingerprint).

create_specie(Population_Id, SpeCon, Fingerprint, SpecieSize) ->
    Specie_Id = genotype:generate_UniqueId(),
    create_specie(Population_Id, Specie_Id, SpecieSize, [], SpeCon, Fingerprint).

% The create_specie/6 function constructs the agents using the genotype:construct_Agent/3 function, accumulating the Agent_Ids in the IdAcc list.
% Once all the agents have been created, the function creates the specie record, fills in the required elements, writes the specie to database,
% and then finally returns the Specie_Id to the caller.
create_specie(Population_Id, Specie_Id, 0, IdAcc, SpeCon, Fingerprint) ->
    ?INFO("Specie_Id:~p Morphology:~p~n", [Specie_Id, SpeCon#constraint.morphology]),
    Specie = #specie{
        id = Specie_Id,
        population_id = Population_Id,
        fingerprint = Fingerprint,
        constraint = SpeCon,
        agent_ids = IdAcc
    },
    genotype:write(Specie),
    Specie_Id;
create_specie(Population_Id, Specie_Id, Agent_Index, IdAcc, SpeCon, Fingerprint) ->
    Agent_Id = {genotype:generate_UniqueId(), agent},
    genotype:construct_Agent(Specie_Id, Agent_Id, SpeCon),
    create_specie(Population_Id, Specie_Id, Agent_Index - 1, [Agent_Id|IdAcc], SpeCon, Fingerprint).

% The function continue/0 and continue/1 are used to summon an already existing population with either the default population Id (test),
% or the specified Population_Id.
continue() ->
    rand:seed(exs64, util:now()),
    S = #state{op_mode = [gt, benchmark]},
    % 开启gen_server进程
    population_monitor:start(test, S).

continue(Population_Id) ->
    rand:seed(exs64, util:now()),
    S = #state{population_id = Population_Id, op_mode = [gt, benchmark]},
    % 开启gen_server进程
    population_monitor:start(Population_Id, S).

% The function mutate_population/4 mutates the agents within every specie in its specie_ids list, maintaining each specie within the size of
% KeepTot. The function first calculates the average cost of each neuron, and then calls each specie seperately with the Fitness_Postprocessor
% and Selection_Algorithm parameters, which are used to mutate the species.
% 说明：在evolutionary_algorithm（进化算法）是generational（世代，等所有人口均完成评估才生成下一代）时使用
mutate_population(Population_Id, KeepTot, Fitness_Postprocessor, Selection_Algorithm) ->
    NeuralEnergyCost = calculate_EnergyCost(Population_Id),
    F = fun() ->
        P = genotype:read({population, Population_Id}),
        Specie_Ids = P#population.specie_ids,
        [mutate_Specie(Specie_Id, KeepTot, NeuralEnergyCost, Fitness_Postprocessor, Selection_Algorithm) || Specie_Id <- Specie_Ids]
    end,
    {atomic, _} = mnesia:transaction(F).

% The function mutate_Specie/5 calls the selection algorithm function to seperate the fit from the unfit organisms in the specie, and then
% mutates the fit organisms to produce offspring, maintaining the total specie size within PopulationLimit. The function first calls the
% fitness_postprocessor which sorts the agent summaries. Then, the resorted updated summaries are split into a valid (fit) and invalid (unfit)
% lists of agents by the selection algorithm. The invalid agents are deleted, and the valid agents are used to create offspring using the
% particular Selection_Algorithm_Name function. The agent ids belonging to the next generation (the valid agents and their offspring) are then
% produced by the selection function. Finally, the innovation factor (the last time the specie's top fitness improved) is updated, the ids of
% the 3 top agents within the species are noted, and the updated specie record is written to database.
mutate_Specie(Specie_Id, PopulationLimit, NeuralEnergyCost, Fitness_Postprocessor_Name, Selection_Algorithm_Name) ->
    S = genotype:dirty_read({specie, Specie_Id}),
    {AvgFitness, Std, MaxFitness, MinFitness} = calculate_SpecieFitness({specie, S}),
    Agent_Ids = S#specie.agent_ids,
    Sorted_AgentSummaries = lists:reverse(lists:sort(construct_AgentSummaries(Agent_Ids, []))),
    % Sorted_AgentSummaries：{Fitness, TotN, Agent_Id}的列表，按适应度降序排列
    ?DBG("Using: Fitness Postprocessor:~p Selection Algorirthm:~p~n", [Fitness_Postprocessor_Name, Selection_Algorithm_Name]),
    ProperlySorted_AgentSummaries = fitness_postprocessor:Fitness_Postprocessor_Name(Sorted_AgentSummaries),
    % ProperlySorted_AgentSummaries：按调整后的适应度降序排列的AgentSummaries
    {NewGenAgent_Ids, TopAgent_Ids} = selection_algorithm:Selection_Algorithm_Name(ProperlySorted_AgentSummaries, NeuralEnergyCost, PopulationLimit), % 3个参数的
    {FList, _TNList, _AgentIds} = lists:unzip3(Sorted_AgentSummaries),
    [TopFitness|_] = FList,
    {Factor, Fitness} = S#specie.innovation_factor,
    U_InnovationFactor = case TopFitness > Fitness of
        true ->
            {0, TopFitness};
        false ->
            {Factor - 1, Fitness}
    end,
    genotype:write(S#specie{
        agent_ids = NewGenAgent_Ids,
        champion_ids = TopAgent_Ids,
        fitness = {AvgFitness, Std, MaxFitness, MinFitness},
        innovation_factor = U_InnovationFactor}).

% The construct_AgentSummaries/2 reads the agents in the Agent_Ids list, and composes a list of tuples of the following format:
% [{AgentFitness,AgentTotNeurons,Agent_Id}...]. This list of tuples is reffered to as AgentSummaries. Once the AgentSummaries list is composed,
% it is returned to the caller.
construct_AgentSummaries([Agent_Id|Agent_Ids], Acc) ->
    A = genotype:dirty_read({agent, Agent_Id}),
    % Summary：{Fitness, TotN, Agent_Id}
    Summary = {A#agent.fitness, length((genotype:dirty_read({cortex, A#agent.cx_id}))#cortex.neuron_ids), Agent_Id},
    construct_AgentSummaries(Agent_Ids, [Summary|Acc]);
construct_AgentSummaries([], Acc) ->
    Acc.

% The create_MutantAgentCopy/1 first creates a clone of the Agent_Id, and then uses the genome_mutator:mutate/1 function to mutate that clone,
% returning the id of the cloned agent to the caller.
create_MutantAgentCopy(Agent_Id) ->
    AgentClone_Id = genotype:clone_Agent(Agent_Id),
    ?DBG("AgentClone_Id:~p~n", [AgentClone_Id]),
    genome_mutator:mutate(AgentClone_Id),
    AgentClone_Id.

% The create_MutantAgentCopy/2 is similar to arity 1 function of the same name, but it also adds the id of the cloned mutant agent to the specie
% record to which the original belonged. The specie with its updated agent_ids is then written to database, and the id of the mutant clone is
% returned to the caller.
% 说明：在evolutionary_algorithm（进化算法）是steady_state（稳态，有人口损失时随时从原型池中挑选原型补充实例）时使用
create_MutantAgentCopy(Agent_Id, safe) ->
    A = genotype:dirty_read({agent, Agent_Id}),
    S = genotype:dirty_read({specie, A#agent.specie_id}),
    AgentClone_Id = genotype:clone_Agent(Agent_Id),
    ?DBG("AgentClone_Id:~p~n", [AgentClone_Id]),
    Agent_Ids = S#specie.agent_ids,
    genotype:write(S#specie{agent_ids = [AgentClone_Id|Agent_Ids]}),
    genome_mutator:mutate(AgentClone_Id),
    AgentClone_Id.

% The delete_population/1 function deletes the entire population, by deleting the specie records belonging to the Population_Id, deleting
% the agent records belonging to those species, and then deleting the population record itself.
delete_population(Population_Id) ->
    P = genotype:dirty_read({population, Population_Id}),
    Specie_Ids = P#population.specie_ids,
    [delete_specie(Specie_Id) || Specie_Id <- Specie_Ids],
    mnesia:delete({population, Population_Id}).

% The delete_specie/1 function deletes the agents associated with the Specie_Id, and then deletes the specie record itself.
delete_specie(Specie_Id) ->
    S = genotype:dirty_read({specie, Specie_Id}),
    Agent_Ids = S#specie.agent_ids,
    [genotype:delete_Agent(Agent_Id) || Agent_Id <- Agent_Ids],
    mnesia:delete({specie, Specie_Id}).

% The calculate_EnergyCost/1 calculates the average cost of each neuron, based on the fitness of each agent in the population, and the
% total number of neurons in the population. The value is calcualted by first adding up all the fitness scores of the agents belonging to
% the population. Then adding up the total number of neurons composing each agent in the population. And then finally producing the EnergyCost
% value by dividing the TotEnergy by TotNeurons, returning the value to the caller.
% 计算平均每个神经元的能量代价（能量即适应度）
calculate_EnergyCost(Population_Id) ->
    Agent_Ids = extract_AgentIds(Population_Id, all),
    TotEnergy = lists:sum([extract_AgentFitness(Agent_Id) || Agent_Id <- Agent_Ids]),
    TotNeurons = lists:sum([extract_AgentTotNeurons(Agent_Id) || Agent_Id <- Agent_Ids]),
    EnergyCost = TotEnergy / TotNeurons,
    EnergyCost.

% The function extract_AgentTotNeurons simply extracts the neuron_ids list, and returns the length of that list, which is the total number of
% neurons belonging to the NN based system.
extract_AgentTotNeurons(Agent_Id) ->
    A = genotype:dirty_read({agent, Agent_Id}),
    Cx = genotype:dirty_read({cortex, A#agent.cx_id}),
    Neuron_Ids = Cx#cortex.neuron_ids,
    length(Neuron_Ids).

extract_AgentFitness(Agent_Id) ->
    A = genotype:dirty_read({agent, Agent_Id}),
    A#agent.fitness.

% 返回各物种的适应度列表
calculate_PopulationFitness(Population_Id, [Specie_Id|Specie_Ids], AvgFAcc, MaxFAcc, MinFAcc) ->
    {AvgFitness, Std, MaxF, MinF} = calculate_SpecieFitness(Specie_Id),
    case get({fitness, Specie_Id}) of
        undefined ->
            put({fitness, Specie_Id}, [{AvgFitness, Std}]);
        PrevGenFitness ->
            put({fitness, Specie_Id}, [{AvgFitness, Std}|PrevGenFitness])
    end,
    calculate_PopulationFitness(Population_Id, Specie_Ids, [{Specie_Id, AvgFitness}|AvgFAcc],
                                [{Specie_Id, MaxF}|MaxFAcc], [{Specie_Id, MinF}|MinFAcc]);
calculate_PopulationFitness(_Population_Id, [], AvgFAcc, MaxFAcc, MinFAcc) ->
    {AvgFAcc, MaxFAcc, MinFAcc}.

% The calculate_SpecieFitness/1 function calculates the general fitness statistic of the specie, the averate, max, min, and standard deviation of
% the specie's fitness. The function first composes a fitness list by accessing the fitness scores of each agent belonging to it, and then
% calculates the noted above statistics from that list, returning the tuple to the caller.
calculate_SpecieFitness({specie, S}) ->
    Agent_Ids = S#specie.agent_ids,
    FitnessAcc = collect_fitness(Agent_Ids),
    Sorted_FitnessAcc = lists:sort(FitnessAcc),
    case Sorted_FitnessAcc of
        [] ->
            MinFitness = 0,
            MaxFitness = 0,
            AvgFitness = 0,
            Std = inf;
        [MinFitness] ->
            MaxFitness = MinFitness,
            AvgFitness = MinFitness,
            Std = inf;
        [MinFitness|_] ->
            [MaxFitness|_] = lists:reverse(Sorted_FitnessAcc),
            AvgFitness = functions:avg(FitnessAcc),
            Std = functions:std(FitnessAcc)
    end,
    {AvgFitness, Std, MaxFitness, MinFitness};
calculate_SpecieFitness(Specie_Id) ->
    S = genotype:dirty_read({specie, Specie_Id}),
    calculate_SpecieFitness({specie, S}).

% The collect_fitness/1 composes a fitness list composed of the fitness values belonging to the agents in the Agent_Ids list. If the agent does
% not yet have a fitness score, if for example it has just been created/mutated but not yet evaluated, it is skipped. The composed fitness list
% is returned to the caller.
collect_fitness(Agent_Ids) ->
    collect_fitness(Agent_Ids, []).

collect_fitness([Agent_Id|Agent_Ids], FitnessAcc) ->
    A = genotype:dirty_read({agent, Agent_Id}),
    case A#agent.fitness of
        undefined ->
            collect_fitness(Agent_Ids, FitnessAcc);
        Fitness ->
            collect_fitness(Agent_Ids, [Fitness|FitnessAcc])
    end;
collect_fitness([], FitnessAcc) ->
    FitnessAcc.

gather_STATS(Population_Id, EvaluationsAcc) ->
    ?INFO("Gathering Species STATS in progress...~n"),
    TimeStamp = util:now(),
    F = fun() ->
        P = genotype:read({population, Population_Id}),
        T = P#population.trace,
        SpecieSTATS = [update_SpecieSTAT(Specie_Id, TimeStamp) || Specie_Id <- P#population.specie_ids],
        % SpecieSTATS：所有物种的stat结构统计数据的列表
        PopulationSTATS = T#trace.stats,
        U_PopulationSTATS = [SpecieSTATS|PopulationSTATS],
        U_TotEvaluations = T#trace.tot_evaluations + EvaluationsAcc,
        U_Trace = T#trace{
            stats = U_PopulationSTATS,
            tot_evaluations = U_TotEvaluations
        },
        ?DBG("Population Trace:~p~n", [U_Trace]),
        mnesia:write(P#population{trace = U_Trace})
    end,
    Result = mnesia:transaction(F),
    ?INFO("Result:~p~n", [Result]).

% 返回某物种的stat结构统计数据
update_SpecieSTAT(Specie_Id, TimeStamp) ->
    Specie_Evaluations = get({evaluations, Specie_Id}),
    put({evaluations, Specie_Id}, 0),
    S = genotype:read({specie, Specie_Id}),
    {Avg_Neurons, Neurons_Std} = calculate_SpecieAvgNodes({specie, S}),
    {AvgFitness, Fitness_Std, MaxFitness, MinFitness} = calculate_SpecieFitness({specie, S}),
    SpecieDiversity = calculate_SpecieDiversity({specie, S}),
    STAT = #stat{
        morphology = (S#specie.constraint)#constraint.morphology,
        specie_id = Specie_Id,
        avg_neurons = Avg_Neurons,
        std_neurons = Neurons_Std,
        avg_fitness = AvgFitness,
        std_fitness = Fitness_Std,
        max_fitness = MaxFitness,
        min_fitness = MinFitness,
        avg_diversity = SpecieDiversity,
        evaluations = Specie_Evaluations,
        time_stamp = TimeStamp
    },
    STATS = S#specie.stats,
    U_STATS = [STAT|STATS],
    mnesia:dirty_write(S#specie{stats = U_STATS}),
    STAT.

calculate_SpecieAvgNodes({specie, S}) ->
    Agent_Ids = S#specie.agent_ids,
    calculate_AvgNodes(Agent_Ids, []);
calculate_SpecieAvgNodes(Specie_Id) ->
    ?DBG("calculate_SpecieAvgNodes(Specie_Id):~p~n", [Specie_Id]),
    S = genotype:read({specie, Specie_Id}),
    calculate_SpecieAvgNodes({specie, S}).

calculate_AvgNodes([Agent_Id|Agent_Ids], NAcc) ->
    %?DBG("calculate_AvgNodes/2 Agent_Id:~p~n", [Agent_Id]),
    A = genotype:read({agent, Agent_Id}),
    Cx = genotype:read({cortex, A#agent.cx_id}),
    Tot_Neurons = length(Cx#cortex.neuron_ids),
    calculate_AvgNodes(Agent_Ids, [Tot_Neurons|NAcc]);
calculate_AvgNodes([], NAcc) ->
    {functions:avg(NAcc), functions:std(NAcc)}.

% 返回各物种的多样性列表
calculate_PopulationDiversity(Population_Id, [Specie_Id|Specie_Ids], Acc) ->
    Diversity = calculate_SpecieDiversity(Specie_Id),
    case get({diversity, Specie_Id}) of
        undefined ->
            put({diversity, Specie_Id}, [Diversity]);
        PrevGenDiversity ->
            put({diversity, Specie_Id}, [Diversity|PrevGenDiversity])
    end,
    calculate_PopulationDiversity(Population_Id, Specie_Ids, [{Specie_Id, Diversity}|Acc]);
calculate_PopulationDiversity(_Population_Id, [], Acc) ->
    Acc.

calculate_SpecieDiversity({specie, S}) ->
    Agent_Ids = S#specie.agent_ids,
    calculate_diversity(Agent_Ids);
calculate_SpecieDiversity(Specie_Id) ->
    S = genotype:dirty_read({specie, Specie_Id}),
    calculate_SpecieDiversity({specie, S}).

calculate_diversity(Agent_Ids) ->
    calculate_diversity(Agent_Ids, []).

% 返回具有不同指纹的agent个数，即多样性
calculate_diversity([Agent_Id|Agent_Ids], DiversityAcc) ->
    A = genotype:read({agent, Agent_Id}),
    Fingerprint = A#agent.fingerprint,
    U_DiversityAcc = (DiversityAcc -- [Fingerprint]) ++ [Fingerprint],
    calculate_diversity(Agent_Ids, U_DiversityAcc);
calculate_diversity([], DiversityAcc) ->
    length(DiversityAcc).

print_SpecieDiversity([Specie_Id|Specie_Ids]) ->
    S = genotype:dirty_read({specie, Specie_Id}),
    Morphology = (S#specie.constraint)#constraint.morphology,
    ?INFO("Specie id:~p~n Specie morphology:~p~n Diversity:~p~n", [Specie_Id, Morphology, get({diversity, Specie_Id})]),
    print_SpecieDiversity(Specie_Ids);
print_SpecieDiversity([]) ->
    done.

