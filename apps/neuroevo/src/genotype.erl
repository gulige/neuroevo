-module(genotype).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

sync() ->
    make:all([load]).

% The population monitor should have all the information with regards to the morphologies and specie constraint under which the agent's genotype
% should be created. Thus the construct_Agent/3 is run with the Specie_Id to which this NN based system will belong, the Agent_Id that this NN based
% intelligent agent will have, and the SpecieCon (specie constraint) that will define the list of activation functions and other parameters from which
% the seed agent can choose its parameters. First the generation is set to 0, since the agent is just created, then the construct_Cortex/3 is ran,
% which creates the NN and returns its Cx_Id. Once the NN is created and the the cortex's id is returned, we can fill out the information needed by
% the agent record, and write it to the mnesia database.
construct_Agent(Specie_Id, Agent_Id, SpecieCon) ->
    rand:seed(exs64, util:now()),
    Generation = 0,
    Encoding_Type = util:random_element(SpecieCon#constraint.agent_encoding_types),
    SPlasticity = util:random_element(SpecieCon#constraint.substrate_plasticities),
    SLinkform = util:random_element(SpecieCon#constraint.substrate_linkforms),
    {Cx_Id, Pattern, Substrate_Id} = construct_Cortex(Agent_Id, Generation, SpecieCon, Encoding_Type, SPlasticity, SLinkform),
    Agent = #agent{
        id = Agent_Id,
        encoding_type = Encoding_Type,
        cx_id = Cx_Id,
        specie_id = Specie_Id,
        constraint = SpecieCon,
        generation = Generation,
        pattern = Pattern,
        tuning_selection_f = util:random_element(SpecieCon#constraint.tuning_selection_fs),
        annealing_parameter = util:random_element(SpecieCon#constraint.annealing_parameters),
        tuning_duration_f = util:random_element(SpecieCon#constraint.tuning_duration_fs),
        perturbation_range = util:random_element(SpecieCon#constraint.perturbation_ranges),
        mutation_operators = SpecieCon#constraint.mutation_operators,
        tot_topological_mutations_f = util:random_element(SpecieCon#constraint.tot_topological_mutations_fs),
        heredity_type = util:random_element(SpecieCon#constraint.heredity_types),
        evo_hist = [],
        substrate_id = Substrate_Id
    },
    write(Agent),
    update_fingerprint(Agent_Id).

% construct_Cortex/3 generates a new Cx_Id, extracts the morphology from the Constraint record passed to it in SpecieCon, and then extracts
% the initial sensors and actuators for that morphology. After the sensors and actuators are extracted, the function calls
% construct_InitialNeuroLayer/7, which creates a single layer of neurons connected to the specified sensors and actuators, and returns
% the ids of the created neurons. Finally, the sensors and actuator ids are extracted from the sensors and actuators, and the cortex record
% is composed and stored to the database.
construct_Cortex(Agent_Id, Generation, SpecieCon, Encoding_Type, SPlasticity, SLinkform) ->
    Cx_Id = {{origin, generate_UniqueId()}, cortex},
    Morphology = SpecieCon#constraint.morphology,
    case Encoding_Type of
        neural ->
            Sensors = [S#sensor{id = {{-1, generate_UniqueId()}, sensor},
                                cx_id = Cx_Id,
                                generation = Generation} ||
                       S <- morphology:get_InitSensors(Morphology)], % 如果想一开始就使用全部的，用morphology:get_Sensors
            Actuators = [A#actuator{id = {{1, generate_UniqueId()}, actuator},
                                    cx_id = Cx_Id,
                                    generation = Generation} ||
                         A <- morphology:get_InitActuators(Morphology)], % 如果想一开始就使用全部的，用morphology:get_Actuators
            N_Ids = construct_InitialNeuroLayer(Cx_Id, Generation, SpecieCon, Sensors, Actuators, [], []),
            S_Ids = [S#sensor.id || S <- Sensors],
            A_Ids = [A#actuator.id || A <- Actuators],
            Cortex = #cortex{
                id = Cx_Id,
                agent_id = Agent_Id,
                neuron_ids = N_Ids,
                sensor_ids = S_Ids,
                actuator_ids = A_Ids
            },
            Substrate_Id = undefined;
        substrate ->
            Substrate_Id = {{void, generate_UniqueId()}, substrate},
            Sensors = [S#sensor{id = {{-1, generate_UniqueId()}, sensor},
                                cx_id = Cx_Id,
                                generation = Generation,
                                fanout_ids = [Substrate_Id]} ||
                       S <- morphology:get_InitSensors(Morphology)], % 如果想一开始就使用全部的，用morphology:get_Sensors
            Actuators = [A#actuator{id = {{1, generate_UniqueId()}, actuator},
                                    cx_id = Cx_Id,
                                    generation = Generation,
                                    fanin_ids = [Substrate_Id]} ||
                         A <- morphology:get_InitActuators(Morphology)], % 如果想一开始就使用全部的，用morphology:get_Actuators
            [write(S) || S <- Sensors],
            [write(A) || A <- Actuators],
            Dimensions = calculate_OptimalSubstrateDimension(Sensors, Actuators),
            Density = 5, % 每一维度内神经节点的个数（5个）
            Depth = 1, % 多少个隐藏超层（1个）
            HyperPlanes = 1, % 隐藏超层内有多少个超平面（1个）
            Densities = [Depth, HyperPlanes | lists:duplicate(Dimensions - 2, Density)], % [X, Y, Z, T...]
            Substrate_CPPs = [CPP#sensor{id = {{-1, generate_UniqueId()}, sensor},
                                         cx_id = Cx_Id,
                                         generation = Generation} ||
                              CPP <- morphology:get_InitSubstrateCPPs(Dimensions, SPlasticity)], % 如果想一开始就使用全部的，用morphology:get_SubstrateCPPs
            Substrate_CEPs = [CEP#actuator{id = {{1, generate_UniqueId()}, actuator},
                                           cx_id = Cx_Id,
                                           generation = Generation} ||
                              CEP <- morphology:get_InitSubstrateCEPs(Dimensions, SPlasticity)],  % 如果想一开始就使用全部的，用morphology:get_SubstrateCEPs
            N_Ids = construct_InitialNeuroLayer(Cx_Id, Generation, SpecieCon, Substrate_CPPs, Substrate_CEPs, [], []),
            ?DBG("Sensors:~p~nActuators:~p~nSubstate_CPPs:~p~nSubstrate_CEPs:~p~n", [Sensors, Actuators, Substrate_CPPs, Substrate_CEPs]),
            S_Ids = [S#sensor.id || S <- Sensors],
            A_Ids = [A#actuator.id || A <- Actuators],
            CPP_Ids = [CPP#sensor.id || CPP <- Substrate_CPPs],
            CEP_Ids = [CEP#actuator.id || CEP <- Substrate_CEPs],
            Substrate = #substrate{
                id = Substrate_Id,
                agent_id = Agent_Id,
                cpp_ids = CPP_Ids,
                cep_ids = CEP_Ids,
                densities = Densities,
                plasticity = SPlasticity,
                link_form = SLinkform
            },
            write(Substrate),
            Cortex = #cortex{
                id = Cx_Id,
                agent_id = Agent_Id,
                neuron_ids = N_Ids,
                sensor_ids = S_Ids,
                actuator_ids = A_Ids
            }
    end,
    write(Cortex),
    {Cx_Id, [{0, N_Ids}], Substrate_Id}.

% construct_InitialNeuroLayer/7 creates a set of neurons for each Actuator in the actuator list. The neurons are initialized in
% the construct_InitialNeurons/6, where they are connected to the actuator, and from a random subset of the sensors passed to the function.
% The construct_InitialNeurons/6 function returns the updated sensors, some of which have now an updated set of fanout_ids which includes
% the new neuron ids they were connected to. The actuator's fanin_ids is then updated to include the neuron ids that were connected to it.
% Once all the actuators have been connected to, the sensors and the actuators are written to the database, and the set of neuron ids
% created within the function is returned to the caller.
construct_InitialNeuroLayer(Cx_Id, Generation, SpecieCon, Sensors, [A|Actuators], AAcc, NIdAcc) ->
    % 每个执行器都新生成一组神经元（vl个）
    N_Ids = [{{0, Unique_Id}, neuron} || Unique_Id <- generate_ids(A#actuator.vl, [])],
    U_Sensors = construct_InitialNeurons(Cx_Id, Generation, SpecieCon, N_Ids, Sensors, A),
    U_A = A#actuator{fanin_ids = N_Ids},
    construct_InitialNeuroLayer(Cx_Id, Generation, SpecieCon, U_Sensors, Actuators, [U_A|AAcc], lists:append(N_Ids, NIdAcc));
construct_InitialNeuroLayer(_Cx_Id, _Generation, _SpecieCon, Sensors, [], AAcc, NIdAcc) ->
    [write(S) || S <- Sensors],
    [write(A) || A <- AAcc],
    NIdAcc.

% construct_InitialNeurons/6 accepts the list of sensors and a single actuator, connects each neuron to the actuator, and randomly chooses
% whether to connect it from all the sensors or a subset of the given sensors. Once all the neurons have been connected to the actuator and
% from the sensors, the updated sensors, whose fanout_ids have been updated with the ids of the neurons, are returned to the caller.
construct_InitialNeurons(Cx_Id, Generation, SpecieCon, [N_Id|N_Ids], Sensors, Actuator) ->
    case rand:uniform() >= 0.5 of
        true ->
            % 50%的概率随机将一个传感器连到该神经元
            S = lists:nth(rand:uniform(length(Sensors)), Sensors),
            U_Sensors = lists:keyreplace(S#sensor.id, 2, Sensors, S#sensor{fanout_ids = [N_Id|S#sensor.fanout_ids]}),
            Input_Specs = [{S#sensor.id, S#sensor.vl}];
        false ->
            % 50%的概率将所有传感器连到该神经元
            U_Sensors = [S#sensor{fanout_ids = [N_Id|S#sensor.fanout_ids]} || S <- Sensors],
            Input_Specs = [{S#sensor.id, S#sensor.vl} || S <- Sensors]
    end,
    construct_Neuron(Cx_Id, Generation, SpecieCon, N_Id, Input_Specs, [Actuator#actuator.id]),
    construct_InitialNeurons(Cx_Id, Generation, SpecieCon, N_Ids, U_Sensors, Actuator);
construct_InitialNeurons(_Cx_Id, _Generation, _SpecieCon, [], Sensors, _Actuator) ->
    Sensors.

% Each neuron record is composed by the construct_Neuron/6 function. The construct_Neuron/6 creates the Input list from the tuples
% [{Id, Weights}...] using the vector lengths specified in the Input_Specs list. The create_InputIdPs/3 function uses create_NeuralWeightsP/2
% to generate a tuple list with random weights in the range of -0.5 to 0.5, and plasticity parameters dependent on the PF function.
% The activation function that the neuron uses is chosen randomly from the neural_afs list within the constraint record passed to
% the construct_Neuron/6 function. construct_Neuron uses calculate_ROIds/3 to extract the list of recursive connection ids from the Output_Ids
% passed to it. Once the neuron record is filled in, it is saved to the database.
construct_Neuron(Cx_Id, Generation, SpecieCon, N_Id, Input_Specs, Output_Ids) ->
    PF = {PFName, NLParameters} = generate_NeuronPF(SpecieCon#constraint.neural_pfs),
    Input_IdPs = create_InputIdPs(PFName, Input_Specs, []),
    Neuron = #neuron{
        id = N_Id,
        cx_id = Cx_Id,
        generation = Generation,
        af = generate_NeuronAF(SpecieCon#constraint.neural_afs),
        pf = PF,
        aggr_f = generate_NeuronAggrF(SpecieCon#constraint.neural_aggr_fs),
        input_idps = Input_IdPs,
        output_ids = Output_Ids,
        ro_ids = calculate_ROIds(N_Id, Output_Ids, [])
    },
    write(Neuron).

create_InputIdPs(PF, [{Input_Id, Input_VL}|Input_IdPs], Acc) ->
    WeightsP = create_NeuralWeightsP(PF, Input_VL, []),
    create_InputIdPs(PF, Input_IdPs, [{Input_Id, WeightsP}|Acc]);
create_InputIdPs(_PF, [], Acc) ->
    Acc.

create_NeuralWeightsP(_PFName, 0, Acc) ->
    Acc;
create_NeuralWeightsP(PFName, Index, Acc) ->
    W = rand:uniform() - 0.5,
    PlasticityParams = plasticity:PFName(weight_parameters),
    create_NeuralWeightsP(PFName, Index - 1, [{W, PlasticityParams}|Acc]).

% The generate_NeuronAF/1 accepts a list of activation function tags, and returns a randomly chosen one.
% If an empty list was passed as the parameter, the function returns the default tanh tag.
generate_NeuronAF([]) ->
    tanh;
generate_NeuronAF(Activation_Functions) ->
    lists:nth(rand:uniform(length(Activation_Functions)), Activation_Functions).

% The generate_NeuronPF/1 accepts a list of plasticity function tags, and returns a randomly chosen one.
% If an empty list was passed as the parameter, the function returns the default none tag.
generate_NeuronPF([]) ->
    {none, []};
generate_NeuronPF(PFNames) ->
    PFName = lists:nth(rand:uniform(length(PFNames)), PFNames),
    NLParameters = plasticity:PFName(neural_parameters), % NL：Neural Level
    {PFName, NLParameters}.

% The generate_NeuronAggrF/1 accepts a list of aggregation function tags, and returns a randomly chosen one.
% If an empty list was passed as the parameter, the function returns the default dot_product tag.
generate_NeuronAggrF([]) ->
    dot_product;
generate_NeuronAggrF(Aggregation_Functions) ->
    lists:nth(rand:uniform(length(Aggregation_Functions)), Aggregation_Functions).

% The function calculate_ROIds/3 accepts as input the Self_Id of the neuron, and the Output_Ids of the elements the neuron connects to.
% Since each element specifies its type and, in the case of neurons, specifies the layer index it belongs to, the function checks
% if the Output_Id's layer index is lower than the Self_Id's layer index, if it is, the output connection is recursive and
% the Output_Id is added to the recursive output list. Once the recursive connection ids have been extracted from the Output_Ids,
% the extracted id list is returned to the caller.
% 计算Output_Ids中哪些是Self_Id的递归连接
calculate_ROIds(Self_Id, [Output_Id|Ids], Acc) ->
    case Output_Id of % SelfId是神经元Id，Output_Id可能是执行器Id或神经元Id
        {_, actuator} ->
            calculate_ROIds(Self_Id, Ids, Acc);
        Output_Id ->
            % LI: Layer Index的缩写
            {{TLI, _}, _NodeType} = Self_Id,
            {{LI, _}, _} = Output_Id,
            case LI =< TLI of % 本层也算
                true ->
                    calculate_ROIds(Self_Id, Ids, [Output_Id|Acc]);
                false ->
                    calculate_ROIds(Self_Id, Ids, Acc)
            end
    end;
calculate_ROIds(_Self_Id, [], Acc) ->
    lists:reverse(Acc).

% The generate_ids/2 function creates a list of unique Ids.
generate_ids(0, Acc) ->
    Acc;
generate_ids(Index, Acc) ->
    Id = generate_UniqueId(),
    generate_ids(Index - 1, [Id|Acc]).

% The generate_UniqueId/0 creates a unique Id using current time, the Id is a floating point value.
generate_UniqueId() ->
    {MegaSeconds, Seconds, MicroSeconds} = util:now(),
    1 / (MegaSeconds * 1000000 + Seconds + MicroSeconds / 1000000).

% 计算最佳的基底维数（依据传感器和执行器的format）
calculate_OptimalSubstrateDimension(Sensors, Actuators) ->
    S_Formats = [S#sensor.format || S <- Sensors],
    A_Formats = [A#actuator.format || A <- Actuators],
    extract_maxdim(S_Formats ++ A_Formats, []) + 2. % 2：一维K（超层），加一维Z（超平面）

% 提取最大维数
extract_maxdim([F|Formats], Acc) ->
    DS = case F of % DS：Dimension Size
        {symmetric, Dims} -> length(Dims);
        no_geo -> 1;
        undefined -> 1
    end,
    extract_maxdim(Formats, [DS|Acc]);
extract_maxdim([], Acc) ->
    lists:max(Acc).

% update_fingerprint/1 calculates the fingerprint of the agent, where the fingerprint is just a tuple of the various general features of
% the NN based system, a list of features that play some role in distinguishing its genotype's general properties from those of other NN systems.
% The fingerprint here is composed of the generalized pattern (pattern minus the unique ids), generalized evolutionary history
% (evolutionary history minus the unique ids of the elements), a generalized sensor set, a generalized actuator set, and topology summary.
update_fingerprint(Agent_Id) ->
    A = read({agent, Agent_Id}),
    %?DBG("A:~p~n", [A]),
    Cx = read({cortex, A#agent.cx_id}),
    % 去除id，达到泛化目的
    GeneralizedSensors = [(read({sensor, S_Id}))#sensor{id = undefined, cx_id = undefined, fanout_ids = []} || S_Id <- Cx#cortex.sensor_ids],
    GeneralizedActuators = [(read({actuator, A_Id}))#actuator{id = undefined, cx_id = undefined, fanin_ids = []} || A_Id <- Cx#cortex.actuator_ids],
    GeneralizedPattern = [{LayerIndex, length(LNIds)} || {LayerIndex, LNIds} <- A#agent.pattern],
    GeneralizedEvoHist = generalize_EvoHist(A#agent.evo_hist, []),
    N_Ids = Cx#cortex.neuron_ids,
    {Tot_Neuron_ILs, Tot_Neuron_OLs, Tot_Neuron_ROs, AF_Distribution} = get_NodeSummary(N_Ids),
    EncodingType = A#agent.encoding_type,
    TopologySummary = #topology_summary{
        type = EncodingType,
        tot_neurons = length(N_Ids),
        tot_n_ils = Tot_Neuron_ILs,
        tot_n_ols = Tot_Neuron_OLs,
        tot_n_ros = Tot_Neuron_ROs,
        af_distribution = AF_Distribution},
    Fingerprint = {GeneralizedPattern, GeneralizedEvoHist, GeneralizedSensors, GeneralizedActuators, TopologySummary},
    write(A#agent{fingerprint = Fingerprint}).

% generalize_EvoHist/2 generalizes the evolutionary history tuples by removing the unique element ids. Two neurons which are using exactly
% the same activation function, located exactly in the same layer, and using exactly the same weights will still have different unique ids,
% thus these ids must be removed to produce a more general set of tuples. There are 3 types of tuples in evo_hist list, with 3, 2 and 1 element ids.
% Once the evolutionary history list is generalized, it is returned to the caller.
% 影响3个元素的变异操作符
generalize_EvoHist([{MO, {{ALI, _AUId}, AType}, {{BLI, _BUId}, BType}, {{CLI, _CUId}, CType}}|EvoHist], Acc) ->
    generalize_EvoHist(EvoHist, [{MO, {ALI, AType}, {BLI, BType}, {CLI, CType}}|Acc]);
% 影响2个元素的变异操作符
generalize_EvoHist([{MO, {{ALI, _AUId}, AType}, {{BLI, _BUId}, BType}}|EvoHist], Acc) ->
    generalize_EvoHist(EvoHist, [{MO, {ALI, AType}, {BLI, BType}}|Acc]);
% 影响1个元素的变异操作符
generalize_EvoHist([{MO, {{ALI, _AUId}, AType}}|EvoHist], Acc) ->
    generalize_EvoHist(EvoHist, [{MO, {ALI, AType}}|Acc]);
% 影响非NN元素的变异操作符
generalize_EvoHist([{MO, _EId}|EvoHist], Acc) ->
    generalize_EvoHist(EvoHist, [{MO}|Acc]);
generalize_EvoHist([], Acc) ->
    lists:reverse(Acc).

get_NNTopologySummary(Agent_Id) ->
    A = read({agent, Agent_Id}),
    Cx_Id = A#agent.cx_id,
    Cx = read({cortex, Cx_Id}),
    N_Ids = Cx#cortex.neuron_ids,
    {Tot_Neuron_ILs, Tot_Neuron_OLs, Tot_Neuron_ROs, AF_Distribution} = get_NodeSummary(N_Ids),
    EncodingType = A#agent.encoding_type,
    TopologySummary = #topology_summary{
        type = EncodingType,
        tot_neurons = length(N_Ids),
        tot_n_ils = Tot_Neuron_ILs,
        tot_n_ols = Tot_Neuron_OLs,
        tot_n_ros = Tot_Neuron_ROs,
        af_distribution = AF_Distribution},
    TopologySummary.

% 返回N_Ids中全部神经元的：{输入连接总数，输出连接总数，递归连接总数，激活函数分布累计数}
get_NodeSummary(N_Ids) ->
    get_NodeSummary(N_Ids, 0, 0, 0, {0, 0, 0, 0, 0, 0, 0, 0, 0}).

get_NodeSummary([N_Id|N_Ids], ILAcc, OLAcc, ROAcc, FunctionDistribution) ->
    N = read({neuron, N_Id}),
    IL_Count = length(N#neuron.input_idps),
    OL_Count = length(N#neuron.output_ids),
    RO_Count = length(N#neuron.ro_ids),
    AF = N#neuron.af,
    {TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin} = FunctionDistribution,
    U_FunctionDistribution = case AF of
        tanh -> {TotTanh + 1, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin};
        sin -> {TotTanh, TotSin + 1, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin};
        cos -> {TotTanh, TotSin, TotCos + 1, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin};
        gaussian -> {TotTanh, TotSin, TotCos, TotGaussian + 1, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin};
        absolute -> {TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute + 1, TotSgn, TotLog, TotSqrt, TotLin};
        sgn -> {TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn + 1, TotLog, TotSqrt, TotLin};
        log -> {TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog + 1, TotSqrt, TotLin};
        sqrt -> {TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt + 1, TotLin};
        linear -> {TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin + 1};
        Other -> ?INFO("Unknown AF, please update AF_Distribution tuple with: ~p~n", [Other])
    end,
    get_NodeSummary(N_Ids, IL_Count + ILAcc, OL_Count + OLAcc, RO_Count + ROAcc, U_FunctionDistribution);
get_NodeSummary([], ILAcc, OLAcc, ROAcc, FunctionDistribution) ->
    {ILAcc, OLAcc, ROAcc, FunctionDistribution}.

% read/1 accepts the tuple composed of a table name and a key: {TableName, Key},
% which it then uses to read from the mnesia database and return the record to the caller.
% 从存储层读数据
% TnK：{Tab, Key}，Table and Key
read(TnK) ->
    case mnesia:read(TnK) of
        [] ->
            undefined;
        [R] ->
            R
    end.

% 从存储层脏读数据
% TnK：{Tab, Key}，Table and Key
dirty_read(TnK) ->
    case mnesia:dirty_read(TnK) of
        [] ->
            undefined;
        [R] ->
            R
    end.

% write/1 accepts a record and writes it to the database.
% 向存储层写数据
% R：Record
write(R) ->
    % Calls the function mnesia:write(Tab, Record, write), where Tab is element(1, Record).
    F = fun() -> mnesia:write(R) end,
    mnesia:transaction(F).

% delete/1 accepts a tuple {TableName, Key}, and deletes the associated record from the table.
% 从存储层删数据
% TnK：{Tab, Key}，Table and Key
delete(TnK) ->
    F = fun() -> mnesia:delete(TnK) end,
    mnesia:transaction(F).

% print/1 accepts an agent's id, and prints out the complete genotype of that agent.
print(Agent_Id) ->
    F = fun() ->
        A = read({agent, Agent_Id}),
        Cx = read({cortex, A#agent.cx_id}),
        io:format("~p~n", [A]),
        io:format("~p~n", [Cx]),
        [io:format("~p~n", [read({sensor, Id})]) || Id <- Cx#cortex.sensor_ids],
        [io:format("~p~n", [read({neuron, Id})]) || Id <- Cx#cortex.neuron_ids],
        [io:format("~p~n", [read({actuator, Id})]) || Id <- Cx#cortex.actuator_ids],
        case A#agent.substrate_id of
            undefined ->
                ok;
            Substrate_Id ->
                Substrate = read({substrate, Substrate_Id}),
                io:format("~p~n", [Substrate]),
                [io:format("~p~n", [read({sensor, Id})]) || Id <- Substrate#substrate.cpp_ids],
                [io:format("~p~n", [read({actuator, Id})]) || Id <- Substrate#substrate.cep_ids]
        end
    end,
    mnesia:transaction(F).

% delete_Agent/1 accepts the id of an agent, and then delets that agent's genotype. This function assumes that the id of the agent will be
% removed from the specie's agent_ids list, and any other clean up procedures, by the calling function.
delete_Agent(Agent_Id) ->
    A = read({agent, Agent_Id}),
    Cx = read({cortex, A#agent.cx_id}),
    [delete({neuron, Id}) || Id <- Cx#cortex.neuron_ids],
    [delete({sensor, Id}) || Id <- Cx#cortex.sensor_ids],
    [delete({actuator, Id}) || Id <- Cx#cortex.actuator_ids],
    delete({cortex, A#agent.cx_id}),
    delete({agent, Agent_Id}),
    case A#agent.substrate_id of
        undefined ->
            ok;
        Substrate_Id ->
            Substrate = read({substrate, Substrate_Id}),
            [delete({sensor, Id}) || Id <- Substrate#substrate.cpp_ids],
            [delete({actuator, Id}) || Id <- Substrate#substrate.cep_ids],
            delete({substrate, Substrate_Id})
    end.

% delete_Agent/2 accepts the id of an agent, and then delets that agent's genotype, but ensures that the specie to which the agent belongs,
% has its agent_ids element updated. Unlike delete_Agent/1, this function updates the specie record.
delete_Agent(Agent_Id, safe) ->
    F = fun() ->
        A = genotype:read({agent, Agent_Id}),
        S = genotype:read({specie, A#agent.specie_id}),
        Agent_Ids = S#specie.agent_ids,
        write(S#specie{agent_ids = lists:delete(Agent_Id, Agent_Ids)}),
        delete_Agent(Agent_Id)
    end,
    Result = mnesia:transaction(F),
    ok.

% clone_Agent/1 is a wrapper for clone_Agent/2, with clone agent id generated.
clone_Agent(Agent_Id) ->
    CloneAgent_Id = {generate_UniqueId(), agent},
    clone_Agent(Agent_Id, CloneAgent_Id).

% clone_Agent/2 accepts Agent_Id and CloneAgent_Id, and then clones the agent, giving the clone CloneAgent_Id. The function first creates
% an ETS table to which it writes the ids of all the elements of the genotype, and their corresponding clone ids. Once all ids and clone ids
% have been generated, the function then begins to clone the actual elements. clone_Agent/2 first clones the neurons using clone_neurons/2,
% then the sensors using clone_sensonrs/2, and finally the actuators using clone_actuators/2. Once these elements are cloned, the function
% writes to database the clone versions of the cortex and the agent records, by writing to databse the original records with updated ids.
clone_Agent(Agent_Id, CloneAgent_Id) ->
    F = fun() ->
        A = read({agent, Agent_Id}),
        Cx = read({cortex, A#agent.cx_id}),
        IdsNCloneIds = ets:new(idsNcloneids, [set, private]), % 临时用
        ets:insert(IdsNCloneIds, {bias, bias}),
        ets:insert(IdsNCloneIds, {Agent_Id, CloneAgent_Id}),
        % 先把id集中克隆出来，供后面取用
        [CloneCx_Id] = map_ids(IdsNCloneIds, [A#agent.cx_id], []),
        CloneN_Ids = map_ids(IdsNCloneIds, Cx#cortex.neuron_ids, []),
        CloneS_Ids = map_ids(IdsNCloneIds, Cx#cortex.sensor_ids, []),
        CloneA_Ids = map_ids(IdsNCloneIds, Cx#cortex.actuator_ids, []),
        case A#agent.substrate_id of
            undefined ->
                clone_neurons(IdsNCloneIds, Cx#cortex.neuron_ids),
                clone_sensors(IdsNCloneIds, Cx#cortex.sensor_ids),
                clone_actuators(IdsNCloneIds, Cx#cortex.actuator_ids),
                U_EvoHist = map_EvoHist(IdsNCloneIds, A#agent.evo_hist),
                % 将被克隆者个体数据结构（cortex、agent）里的id替换为克隆者的相关id，并写入数据库
                write(Cx#cortex{
                    id = CloneCx_Id,
                    agent_id = CloneAgent_Id,
                    sensor_ids = CloneS_Ids,
                    actuator_ids = CloneA_Ids,
                    neuron_ids = CloneN_Ids
                }),
                write(A#agent{
                    id = CloneAgent_Id,
                    cx_id = CloneCx_Id,
                    evo_hist = U_EvoHist
                });
            Substrate_Id ->
                Substrate = read({substrate, A#agent.substrate_id}),
                [CloneSubstrate_Id] = map_ids(IdsNCloneIds, [A#agent.substrate_id], []),
                CloneCPP_Ids = map_ids(IdsNCloneIds, Substrate#substrate.cpp_ids, []),
                CloneCEP_Ids = map_ids(IdsNCloneIds, Substrate#substrate.cep_ids, []),
                clone_neurons(IdsNCloneIds, Cx#cortex.neuron_ids),
                clone_sensors(IdsNCloneIds, Cx#cortex.sensor_ids),
                clone_actuators(IdsNCloneIds, Cx#cortex.actuator_ids),
                clone_sensors(IdsNCloneIds, Substrate#substrate.cpp_ids),
                clone_actuators(IdsNCloneIds, Substrate#substrate.cep_ids),
                U_EvoHist = map_EvoHist(IdsNCloneIds, A#agent.evo_hist),
                % 将被克隆者个体数据结构（substrate、cortex、agent）里的id替换为克隆者的相关id，并写入数据库
                write(Substrate#substrate{
                    id = CloneSubstrate_Id,
                    agent_id = CloneAgent_Id,
                    cpp_ids = CloneCPP_Ids,
                    cep_ids = CloneCEP_Ids
                }),
                write(Cx#cortex{
                    id = CloneCx_Id,
                    agent_id = CloneAgent_Id,
                    sensor_ids = CloneS_Ids,
                    actuator_ids = CloneA_Ids,
                    neuron_ids = CloneN_Ids
                }),
                write(A#agent{
                    id = CloneAgent_Id,
                    cx_id = CloneCx_Id,
                    substrate_id = CloneSubstrate_Id,
                    evo_hist = U_EvoHist
                })
        end,
        ets:delete(IdsNCloneIds) % 辅助用，用完删除
    end,
    mnesia:transaction(F),
    CloneAgent_Id.

% map_ids/3 accepts the name of the ets table, and a list of ids. It then goes through every id and creates a clone version of the id
% by generating a new unique id. The function is able to generate new id structures for neuron, cortex, sensor and actuator id types.
map_ids(TableName, [Id|Ids], Acc) ->
    CloneId = case Id of
        {{LayerIndex, _UId}, Type} ->
            {{LayerIndex, generate_UniqueId()}, Type};
        {_UId, Type} ->
            {generate_UniqueId(), Type}
    end,
    ets:insert(TableName, {Id, CloneId}),
    map_ids(TableName, Ids, [CloneId|Acc]);
map_ids(_TableName, [], Acc) ->
    Acc.

% clone_neuron/2 accepts as input the name of the ets table and the list of neuron ids. It then goes through every neuron id, reads the neuron
% from the database, and updates all the ids (id, cx_id, output_ids, ro_ids) and input_idps from their original values, to their clone values
% stored in the ets table. Once the everything is updated, the new (clone) version of the neuron is written to the database.
clone_neurons(TableName, [N_Id|N_Ids]) ->
    N = read({neuron, N_Id}),
    CloneN_Id = ets:lookup_element(TableName, N_Id, 2),
    CloneCx_Id = ets:lookup_element(TableName, N#neuron.cx_id, 2),
    CloneInput_IdPs = [{ets:lookup_element(TableName, I_Id, 2), WeightsP} || {I_Id, WeightsP} <- N#neuron.input_idps],
    CloneInput_IdPs_Modulation = [{ets:lookup_element(TableName, I_Id, 2), WeightsP} || {I_Id, WeightsP} <- N#neuron.input_idps_modulation],
    CloneOutput_Ids = [ets:lookup_element(TableName, O_Id, 2) || O_Id <- N#neuron.output_ids],
    CloneRO_Ids = [ets:lookup_element(TableName, RO_Id, 2) || RO_Id <- N#neuron.ro_ids],
    write(N#neuron{
        id = CloneN_Id,
        cx_id = CloneCx_Id,
        input_idps = CloneInput_IdPs,
        input_idps_modulation = CloneInput_IdPs_Modulation,
        output_ids = CloneOutput_Ids,
        ro_ids = CloneRO_Ids
    }),
    clone_neurons(TableName, N_Ids);
clone_neurons(_TableName, []) ->
    done.

% clone_sensors/2 accepts as input the name of the ets table and the list of sensor ids. It then goes through every sensor id, reads the sensor
% from the database, and updates all the ids (id, cx_id, and fanout_ids) from their original values, to their clone values stored in the ets table.
% Then the new version of the sensor is written to the database.
clone_sensors(TableName, [S_Id|S_Ids]) ->
    S = read({sensor, S_Id}),
    CloneS_Id = ets:lookup_element(TableName, S_Id, 2),
    CloneCx_Id = ets:lookup_element(TableName, S#sensor.cx_id, 2),
    CloneFanout_Ids = [ets:lookup_element(TableName, Fanout_Id, 2) || Fanout_Id <- S#sensor.fanout_ids],
    write(S#sensor{
        id = CloneS_Id,
        cx_id = CloneCx_Id,
        fanout_ids = CloneFanout_Ids
    }),
    clone_sensors(TableName, S_Ids);
clone_sensors(_TableName, []) ->
    done.

% clone_actuators/2 accepts as input the name of the ets table and the list of actuator ids. It then goes through every actuator id, reads the actuator
% from the database, and updates all the ids (id, cx_id, and fanin_ids) from their original values, to their clone values stored in the ets table.
% Then the new version of the actuator is written to the database.
clone_actuators(TableName, [A_Id|A_Ids]) ->
    A = read({actuator, A_Id}),
    CloneA_Id = ets:lookup_element(TableName, A_Id, 2),
    CloneCx_Id = ets:lookup_element(TableName, A#actuator.cx_id, 2),
    CloneFanin_Ids = [ets:lookup_element(TableName, Fanin_Id, 2) || Fanin_Id <- A#actuator.fanin_ids],
    write(A#actuator{
        id = CloneA_Id,
        cx_id = CloneCx_Id,
        fanin_ids = CloneFanin_Ids
    }),
    clone_actuators(TableName, A_Ids);
clone_actuators(_TableName, []) ->
    done.

% map_EvoHist/2 is a wrapper for map_EvoHist/3, which in turn accepts the evo_hist list containing the mutation operator tuples that have been
% appplied to the NN system. The function is used when a clone of a NN system is created. The function updates the original Ids of the elements
% the mutation oeprators have been applied to, to the clone's Ids, so that the updated evo_hist can reflect the clone's topology, as if
% the mutation operators have been applied to it, and that it is not a clone. Once all the tuples in the evo_hist have been updated with
% the clone element ids, the list is reverted to its proper order, and the updated list is returned to the caller.
% 为保持一致性，将所继承的祖先（被克隆者）的进化历史列表中的相关id替换为自己（克隆者）的，仿佛这些MO是作用在克隆者身上一样（而实际上是直接继承过来的）
map_EvoHist(TableName, EvoHist) ->
    map_EvoHist(TableName, EvoHist, []).

map_EvoHist(TableName, [{MO, E1Id, E2Id, E3Id}|EvoHist], Acc) ->
    Clone_E1Id = ets:lookup_element(TableName, E1Id, 2),
    Clone_E2Id = ets:lookup_element(TableName, E2Id, 2),
    Clone_E3Id = ets:lookup_element(TableName, E3Id, 2),
    map_EvoHist(TableName, EvoHist, [{MO, Clone_E1Id, Clone_E2Id, Clone_E3Id}|Acc]);
map_EvoHist(TableName, [{MO, E1Id, E2Id}|EvoHist], Acc) ->
    Clone_E1Id = ets:lookup_element(TableName, E1Id, 2),
    Clone_E2Id = ets:lookup_element(TableName, E2Id, 2),
    map_EvoHist(TableName, EvoHist, [{MO, Clone_E1Id, Clone_E2Id}|Acc]);
map_EvoHist(TableName, [{MO, E1Id}|EvoHist], Acc) ->
    Clone_E1Id = ets:lookup_element(TableName, E1Id, 2),
    map_EvoHist(TableName, EvoHist, [{MO, Clone_E1Id}|Acc]);
map_EvoHist(_TableName, [], Acc) ->
    lists:reverse(Acc).

% 形成物种
% The function speciate/1 reads a newly created agent record, calculates that agent's fingerprint, and then based on that fingerprint
% either inserts it into an already existing specie, or creates a new specie of which the agent is the first of a kind. The function
% first creates the fingerprint of the agent using the genotype:update_fingerprint/1 function. Then the function checks whether this is
% a test agent, in which case it is only used for testing, and does not belong to any specie or population. If the agent is not a test agent,
% then the specie and population to which its parent belonged is retreived from the database (the specie and population ids are conserved
% in the offspring during mutation, so the agent already holds his parent's specie and population ids). Afterwards, a specie is found
% which has the same fingerprint as the agent. If there is no such specie, then a new specie is created, a specie that belongs to
% the same population as the agent, and has the same constriants and fingerprint as the agent fathering the specie. Then the agent's id
% is entered into the specie, and the updated specie and agent are written to database. If on the other hand a specie already exists with
% the same fingerprint as the agent, then the agent's id is added to the existing specie, and the updated specie and agent are written to database.
speciate(Agent_Id) ->
    update_fingerprint(Agent_Id),
    A = read({agent, Agent_Id}),
    case A#agent.id of
        test -> % test agent belongs to no specie and no population
            write(A#agent{fitness = undefined});
        _ ->
            Parent_S = read({specie, A#agent.specie_id}), % 所属物种
            P = read({population, Parent_S#specie.population_id}), % 所属群落
            case [Id || Id <- P#population.specie_ids, (read({specie, Id}))#specie.fingerprint == A#agent.fingerprint] of
                [] -> % 在所属群落的已有物种里，找不到与该个体指纹相同的物种，则创建一个新物种记录
                    Specie_Id = population_monitor:create_specie(P#population.id, A#agent.constraint, A#agent.fingerprint),
                    S = read({specie, Specie_Id}),
                    U_A = A#agent{specie_id = Specie_Id, fitness = undefined},
                    U_S = S#specie{agent_ids = [Agent_Id]},
                    write(U_A),
                    write(U_S);
                [Specie_Id] ->
                    S = read({specie, Specie_Id}),
                    U_A = A#agent{specie_id = Specie_Id, fitness = undefined},
                    U_S = S#specie{agent_ids = [Agent_Id|S#specie.agent_ids]},
                    write(U_A),
                    write(U_S)
            end
    end.

% test/0 performs a test of the standard functions of the genotype module, by first creating a new agent, then cloning that agent,
% then printing the genotype of the original agent and its clone, and then finally deleting both of the agents.
test() ->
    Specie_Id = test,
    Agent_Id = test,
    CloneAgent_Id = test_clone,
    SpecieCon = #constraint{morphology = pole_balancing,
                            connection_architecture = feedforward,
                            population_evo_alg_f = generational,
                            neural_afs = [tanh],
                            agent_encoding_types = [substrate],
                            substrate_plasticities = [none]},
    F = fun() ->
        construct_Agent(Specie_Id, Agent_Id, SpecieCon),
        clone_Agent(Specie_Id, CloneAgent_Id),
        print(Agent_Id),
        print(CloneAgent_Id),
        delete_Agent(Agent_Id),
        delete_Agent(CloneAgent_Id)
    end,
    mnesia:transaction(F).

% create_test/0 creates a simple NN based agent using the default constraint record. The function first checks if an agent with the id 'test'
% already exists, if it does, the function deletes that agent and creates a new one. Otherwise, the function just creates a brand new agent
% with the 'test' id.
% genome_mutator测试用
create_test() ->
    Specie_Id = test,
    Agent_Id = test,
    SpecieCon = #constraint{morphology = pole_balancing,
                            connection_architecture = feedforward,
                            population_evo_alg_f = generational,
                            neural_afs = [tanh],
                            agent_encoding_types = [substrate],
                            substrate_plasticities = [iterative]},
    F = fun() ->
        case genotype:read({agent, Agent_Id}) of
            undefined ->
                construct_Agent(Specie_Id, Agent_Id, SpecieCon),
                print(Agent_Id);
            _ ->
                delete_Agent(Agent_Id),
                construct_Agent(Specie_Id, Agent_Id, SpecieCon),
                print(Agent_Id)
        end
    end,
    mnesia:transaction(F).

