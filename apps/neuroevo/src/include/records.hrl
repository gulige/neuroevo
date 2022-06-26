%%% 传感器
-record(sensor, {
    id, % {{-1::LayerCoordinate, float()::UniqueId()}, sensor}
    name, % atom()，获取传感数据时执行的函数名
    type, % standard | substrate
    cx_id, % cortex.id
    scape, % {private|public, atom()::ScapeName}，在morphologies目录下的具体形态中设定
    vl, % int()，传感信号的向量长度
    fanout_ids = [], % [neuron.id...] | [substrate.id]
    generation, % int()
    format, % no_geo | {symmetric, [int()::Resolution...]即[超平面第N维的密度, 超平面第N-1维的密度, ...]}
    parameters, % [any()...]
    phys_rep, % [any()...]
    vis_rep, % [any()...]
    pre_f, % atom()::FunctionName
    post_f % atom()::FunctionName
}).

%%% 执行器
-record(actuator, {
    id, % {{1::LayerCoordinate, float()::UniqueId()}, actuator}
    name, % atom()，作用于环境时执行的函数名
    type, % standard | substrate
    cx_id, % cortex.id
    scape, % {private|public, atom()::ScapeName}，在morphologies目录下的具体形态中设定
    vl, % int()，作用于环境的驱动信号向量长度
    fanin_ids = [], % [neuron.id...] | [substrate.id]
    generation, % int()
    format, % no_geo | {symmetric, [int()::Resolution...]即[超平面第N维的密度, 超平面第N-1维的密度, ...]}
    parameters, % [any()...]
    phys_rep, % [any()...]
    vis_rep, % [any()...]
    pre_f, % atom()::FunctionName
    post_f % atom()::FunctionName
}).

%%% 神经元
% 可塑性可以体现在两个地方：1. 整个神经元级别的，即pf中的ParameterList，2. 权重级别的，即input_idps中每个权重对应的ParameterList
-record(neuron, {
    id, % {{float()::LayerCoordinate, float()::UniqueId}, neuron}
    generation, % int()
    cx_id, % cortex.id
    af, % atom()::FunctionName，激活函数
    pf, % {atom()::FunctionName, any()::ParameterList}，可塑性函数
    aggr_f, % atom()::FunctionName，聚合函数
    input_idps = [], % [{InputId, WeightsP}...]，[{neuron.id|sensor.id|bias, [{float()::Weight, any()::ParameterList}...]}...]
    input_idps_modulation = [],
    output_ids = [], % [neuron.id|actuator.id...]
    ro_ids = [] % [neuron.id...]
}).

%%% 皮质，cortex
-record(cortex, {
    id, % {{origin, float()::UniqueId()}, cortex}
    agent_id, % agent.id
    neuron_ids = [], % [neuron.id...]
    sensor_ids = [], % [sensor.id...]
    actuator_ids = [] % [actuator.id...]
}).

%%% 基底
-record(substrate, {
    id, % 基底id
    agent_id, % NN代理id
    densities, % 密度，只与隐藏超层有关
    % densities格式：[隐藏（非输入、非输出）超层的深度（即有多少个隐藏超层）, 隐藏超层第N维的密度（即含了多少个超平面）, 隐藏超层第N-1维（超平面的最后一维）的密度, ...]
    link_form, % [l2l_feedforward, fully_interconnected, jordan_recurrent, freeform]
    plasticity = none,
    cpp_ids = [], % coordinate pre-processor id list
    cep_ids = [] % connectivity expression producer id list
}).

%%% NN代理
-record(agent, {
    id, % {float()::UniqueId(), agent}
    encoding_type, % atom()::neural|substrate
    generation, % int()
    population_id, % population.id
    specie_id, % specie.id
    cx_id, % cortex.id
    fingerprint, % fingerprint()
    constraint, % constraint()
    evo_hist = [], % [OperatorApplied...]
    % {atom()::MO_Name, ElementA.id, ElementB.id, ElementC.id}
    % {atom()::MO_Name, ElementA.id, ElementB.id}
    % {atom()::MO_Name, ElementA.id}
    fitness = 0, % float()
    innovation_factor = 0, % int()
    pattern = [], % [{float()::LayerCoordinate, N_Ids}...]，拓扑结构（不含连接）
    tuning_selection_f, % atom()::FunctionName
    annealing_parameter, % float()，刚刚加入系统的神经元需要更强的调整，随着时间逐代衰减幅度，用于tuning selection
    tuning_duration_f, % {atom()::FunctionName, any()::Parameter}
    perturbation_range, % float()
    mutation_operators, % [{atom()::FunctionName, float()}...]
    tot_topological_mutations_f, % {atom()::FunctionName, float()}，用于计算发生多少次拓扑结构变异的函数
    heredity_type,
    substrate_id
}).

%%% 指纹，fingerprint
% generalized_sensors = [sensor()::init...]
%    sensor.id = undefined
%    sensor.cx_id = undefined
%    sensor.fanout_ids = []
% generlized_actuators = [actuator()::init...]
%    actuator.id = undefined
%    actuator.cx_id = undefined
%    actuator.fanin_ids = []
% generalized_pattern = [{float()::LayerCoordinate, int()::TotNeurons}...]
% generalized_evohist = [GeneralizedOperatorApplied...]
%    {atom()::MO_Name},
%    {atom()::MO_Name, {float()::ElementA_LayerCoordinate, atom()::ElementA_Type}},
%    {atom()::MO_Name, {float()::ElementA_LayerCoordinate, atom()::ElementA_Type}, {ElementB_LayerCoordinate, ElementB_Type}},
%    {atom()::MO_Name, {float()::ElementA_LayerCoordinate, atom()::ElementA_Type}, {ElementB_LayerCoordinate, ElementB_Type}, {ElementC_LayerCoordinate, ElementC_Type}}

%%% 约束
-record(constraint, {
    morphology = undefined,
    connection_architecture = recurrent, % recurrent|feedforward
    neural_afs = [tanh, cos, gaussian, absolute], % [tanh, cos, gaussian, absolute, sin, sqrt, sigmoid]
    neural_pfs = [none], % [none, hebbian_w, hebbian, ojas_w, ojas,
                         % self_modulationV1, self_modulationV2, self_modulationV3, self_modulationV4, self_modulationV5, self_modulationV6,
                         % neuromodulation]
    substrate_plasticities = [none],
    substrate_linkforms = [l2l_feedforward], % [l2l_feedforward, jordan_recurrent, fully_interconnected]
    neural_aggr_fs = [dot_product], % [dot_product, mult_product, diff]
    tuning_selection_fs = [dynamic_random], % [all, all_random, recent, recent_random, lastgen, lastgen_random]
    tuning_duration_fs = [{const, 10}], % [{const, 20}, {nsize_proportional, 0.5}, {nweight_proportional, 0.5}...]
    annealing_parameters = [1], % [1, 0.9]
    perturbation_ranges = [1], % [0.5, 1, 2, 3...]
    agent_encoding_types = [neural], % [neural, substrate]
    heredity_types = [darwinian], % [darwinian, lamarckian]，达尔文剥离掉学习的影响，而拉马克则可以遗传经验
    mutation_operators = [{mutate_weights, 1}, {add_bias, 1}, {remove_bias, 1}, {mutate_af, 1},
                          {add_outlink, 1}, {add_inlink, 1}, {add_neuron, 1}, {outsplice, 1},
                          {add_sensor, 1}, {add_actuator, 1}, {mutate_plasticity_parameters, 1},
                          {add_cpp, 1}, {add_cep, 1}], % [{atom()::FunctionName, float()}...]，后面的浮点数为SliceSize，即概率点数
                       % [{mutate_weights, 1}, {add_bias, 1}, {remove_bias, 1}, {mutate_af, 1},
                       %  {add_outlink, 1}, {remove_outlink, 1}, {add_inlink, 1}, {remove_inlink, 1},
                       %  {add_sensorlink, 1}, {add_actuatorlink, 1}, {add_neuron, 1}, {remove_neuron, 1},
                       %  {outsplice, 1}, {insplice, 1}, {add_sensor, 1}, {remove_sensor, 1},
                       %  {add_actuator, 1}, {remove_actuator, 1}, {mutate_plasticity_parameters, 1}]
    tot_topological_mutations_fs = [{ncount_exponential, 0.5}], % [{ncount_exponential, 0.5}, {ncount_linear, 1}]
    population_evo_alg_f = generational, % [generational, steady_state]
    population_fitness_postprocessor_f = size_proportional, % [none, size_proportional]
    population_selection_f = competition % [competition, top3]
}).

%%% 物种
-record(specie, {
    id, % atom()|{float()::UniqueId, specie}
    population_id, % population.id
    fingerprint, % fingerprint()
    constraint, % constraint()
    agent_ids = [], % [agent.id...]
    dead_pool = [], % [agent.id...]
    champion_ids = [], % [agent.id..]
    fitness, % float()
    innovation_factor = {0, 0}, % {int(), int()}
    stats = []
}).

%%% 每一轮run的跟踪记录
-record(trace, {
    stats = [], % stat记录的列表
    tot_evaluations = 0,
    step_size = 500
}).

%%% 群落
-record(population, {
    id, % atom()|{float()::UniqueId, population}
    polis_id, % polis.id
    specie_ids = [], % [specie.id...]
    morphologies = [], % [atom()::Morphology_Name...]
    innovation_factor, % int()
    evo_alg_f, % atom()::FunctionName
    fitness_postprocessor_f, % atom()::FunctionName
    selection_f, % atom()::FunctionName
    trace = #trace{}
}).

%%% 城邦
-record(polis, {
    id, % atom()|float()|{float()::UniqueId, polis}|{atom()::PolisName, polis}
    scape_ids = [],
    population_ids = [],
    specie_ids = [],
    dx_ids = [],
    parameters = []
}).

%%% 拓扑摘要
-record(topology_summary, {
    type, % atom()::neural|substrate，编码类型
    tot_neurons,
    tot_n_ils,
    tot_n_ols,
    tot_n_ros,
    af_distribution
}).

%%% benchmark需要的数据结构--->
%

%%% 人口管理参数（Population Management Parameters）
-record(pmp, {
    op_mode = gt, % 运作模式：standard，throughput，gt（genetic tuning）
    population_id = test,
    survival_percentage = 0.5, % 选择阶段，允许多少比例的人口存活下去
    specie_size_limit = 10, % 每一个物种内的原型数量限制
    init_specie_size = 10, % 初始的物种内的原型数量
    polis_id = mathema, % 城邦id
    generation_limit = 100,
    evaluations_limit = 100000,
    fitness_goal = inf,
    benchmarker_pid
}).

%%% 实验
-record(experiment, {
    id,
    backup_flag = true,
    pm_parameters, % pmp
    init_constraints,
    progress_flag = in_progress,
    trace_acc = [], % trace记录的列表
    run_index = 1, % 正在跑第几轮
    tot_runs = 10,
    notes,
    started = {date(), time()}, % 开始时间
    completed, % 结束时间
    interruptions = []
}).

%% 每一个物种的统计记录
%% std表示标准差，又叫均方差，形式上和均方误差MSE的算术平方根（即均方根误差RMSE）一致
%% 之所以强调只在形式上一致，是因为均方差是与均值的关系，而均方误差是与真实值的关系
-record(stat, {
    morphology,
    specie_id,
    avg_neurons,
    std_neurons,
    avg_fitness,
    std_fitness,
    max_fitness,
    min_fitness,
    avg_diversity,
    evaluations,
    time_stamp
}).

%
%%% benchmark需要的数据结构<---

