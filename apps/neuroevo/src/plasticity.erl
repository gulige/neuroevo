%% 三种可塑性学习规则：
%% Hebb:    DW = HIO
%% Oja:     DW = H(I - OW)O
%% 一般Hebb: DW = H(AIO + BI + CO + D)
%% 一般Hebb规则在神经调制中使用

-module(plasticity).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

-define(SAT_LIMIT, math:pi() * 2).

% none/1 returns a set of learning parameters needed by the none/1 plasticity function. Since this function specifies that the neuron has no
% plasticity, the parameter lists are empty. When executed with the {N_Id,mutate} parameter, the function exits, since there is nothing to mutate.
% The exit allows for the neuroevolutionary system to try another mutation operator on the NN system.
% 为神经元级别生成学习参数
none(neural_parameters) ->
    [];
% 为权重级别生成学习参数
none(weight_parameters) ->
    [];
% 扰动学习参数，返回neuron结构
none({_N_Id, mutate}) ->
    exit("Neuron does not support plasticity.").

% none/4 returns the original Input_PIdPs to the caller.
% 根据学习规则调整神经元连接权重
none(_NeuralParameters, _IAcc, Input_PIdPs, _Output) ->
    Input_PIdPs.

%%====================================================================
%% Hebb（修改版）：基于活动历史调整连接权重
%%====================================================================

% hebbian_w/1 function produces the necessary parameter list for the hebbian_w learning rule to operate. The parameter list for the simple hebbian_w
% learning rule is a parameter list composed of a single parameter H: [H], for every synaptic weight of the neuron. When hebbian_w/1 is called with
% the parameter neural_parameters, it returns []. When hebbian_w/1 is executed with the {N_Id,mutate} parameter, the function goes through every
% parameter in the neuron's input_idps, and perturbs the parameter value using the specified spread (?SAT_LIMIT).
% hebbian_w：修改版的Hebb学习规则，每个W有一个自己的H
% 神经元级别没有学习参数
hebbian_w(neural_parameters) ->
    [];
% 权重级别的学习参数为：[H]
hebbian_w(weight_parameters) ->
    [(rand:uniform() - 0.5)]; % 当随机生成的H < 0时，为反Hebb规则，即I*O < 0时权重增加
% 扰动学习参数
hebbian_w({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    InputIdPs = N#neuron.input_idps,
    U_InputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
    N#neuron{input_idps = U_InputIdPs}.

perturb_parameters(InputIdPs, Spread) ->
    TotParameters = lists:sum([ lists:sum([length(Ps) || {_W, Ps} <- WPs]) || {_Input_Id, WPs} <- InputIdPs]),
    MutationProb = 1 / math:sqrt(TotParameters),
    [{Input_Id, [{W, perturb(Ps, MutationProb, Spread, [])} || {W, Ps} <- WPs]} || {Input_Id, WPs} <- InputIdPs].

perturb([Val|Vals], MutationProb, Spread, Acc) ->
    case rand:uniform() < MutationProb of
        true ->
            % 对Val在-Spread到Spread之间进行扰动
            U_Val = functions:saturation((rand:uniform() - 0.5) * 2 * Spread + Val, Spread),
            perturb(Vals, MutationProb, Spread, [U_Val|Acc]);
        false ->
            perturb(Vals, MutationProb, Spread, [Val|Acc])
    end;
perturb([], _MutationProb, _Spread, Acc) ->
    lists:reverse(Acc).

% hebbian_w/4 function operates on each Input_PIdP, calling the hebbrule_w/4 function which processes each of the complementary Is and WPs lists,
% producing the Updated_WPs list in return, with the updated/adapted weights based on the hebbian_w learning rule.
% 根据学习规则调整神经元连接权重
hebbian_w(_NeuralParameters, IAcc, Input_PIdPs, Output) ->
    hebbian_w1(IAcc, Input_PIdPs, Output, []).

hebbian_w1([{IPId, Is}|IAcc], [{IPId, WPs}|Input_PIdPs], Output, Acc) ->
    Updated_WPs = hebbrule_w(Is, WPs, Output, []),
    hebbian_w1(IAcc, Input_PIdPs, Output, [Updated_WPs|Acc]);
hebbian_w1([], [], _Output, Acc) ->
    lists:reverse(Acc);
hebbian_w1([], [{bias, WPs}], _Output, Acc) ->
    lists:reverse([{bias, WPs}|Acc]).

% hebbrule_w/4 applies the hebbian learning rule to each weight, using the input value I, the neuron's calculated output Output, and its own distinct
% learning parameter H associated with each synaptic weight.
hebbrule_w([I|Is], [{W, [H]}|WPs], Output, Acc) ->
    Updated_W = functions:saturation(W + H * I * Output, ?SAT_LIMIT),
    hebbrule_w(Is, WPs, Output, [{Updated_W, [H]}|Acc]);
hebbrule_w([], [], _Output, Acc) ->
    lists:reverse(Acc).

%%====================================================================
%% Hebb（标准版）：基于活动历史调整连接权重
%%====================================================================

% hebbian/1 function produces the necessary parameter list for the hebbian learning rule to operate. The parameter list for the standard hebbian
% learning rule is a parameter list composed of a single parameter H: [H], used by the neuron for all its synaptic weights. When hebbian/1 is called
% with the parameter weight_parameters, it returns []. When the function is executed with the {N_Id,mutate} parameter, it uses the perturb/4 function
% to perturb the parameter list, which in this case is a list composed of a single floating point parameter.
% hebbian：标准版的Hebb学习规则，是全局一个H，而不是每个W有一个自己的H
% 神经元级别的学习参数为：[H]
hebbian(neural_parameters) ->
    [(rand:uniform() - 0.5)]; % 当随机生成的H < 0时，为反Hebb规则，即I*O < 0时权重增加
% 权重级别没有学习参数
hebbian(weight_parameters) ->
    [];
% 扰动学习参数
hebbian({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    {PFName, ParameterList} = N#neuron.pf,
    Spread = ?SAT_LIMIT * 10, % 神经元级别的扰动幅度为10倍
    MutationProb = 1 / math:sqrt(length(ParameterList)),
    U_ParameterList = perturb(ParameterList, MutationProb, Spread, []),
    U_PF = {PFName, U_ParameterList},
    N#neuron{pf = U_PF}.

% hebbian/4 function operates on each Input_PIdP, calling the hebbrule/5 function which processes each of the complementary Is and WPs lists,
% producing the Updated_WPs list in return, with the updated/adapted weights based on the standard hebbian learning rule, using the neuron's
% single learning parameter H.
% 根据学习规则调整神经元连接权重（_M是前向神经元的输出作为调制信号输入，这里无效，纯粹兼容）
hebbian([_M, H], IAcc, Input_PIdPs, Output) ->
    hebbian(H, IAcc, Input_PIdPs, Output, []).

hebbian(H, [{IPId, Is}|IAcc], [{IPId, WPs}|Input_PIdPs], Output, Acc) ->
    Updated_WPs = hebbrule(H, Is, WPs, Output, []),
    hebbian(H, IAcc, Input_PIdPs, Output, [{IPId, Updated_WPs}|Acc]);
hebbian(_H, [], [], _Output, Acc) ->
    lists:reverse(Acc);
hebbian(_H, [], [{bias, WPs}], _Output, Acc) ->
    lists:reverse([{bias, WPs}|Acc]).

% hebbrule/5 applies the hebbian learning rule to each weight, using the input value I, the neuron's calculated output Output, and the neuron's
% leraning parameter H.
hebbrule(H, [I|Is], [{W, []}|WPs], Output, Acc) ->
    Updated_W = functions:saturation(W + H * I * Output, ?SAT_LIMIT),
    hebbrule(H, Is, WPs, Output, [{Updated_W, []}|Acc]);
hebbrule(_H, [], [], _Output, Acc) ->
    lists:reverse(Acc).

%%====================================================================
%% Oja（修改版）：基于活动历史调整连接权重，Oja对权重的调整可自我稳定
%%====================================================================

% ojas_w/1 function produces the necessary parameter list for the oja's learning rule to operate. The parameter list for oja's learning rule is
% a list composed of a single parameter H: [H] per synaptic weight. If the learning parameter is positive, then the postsynaptic neuron's synaptic
% weight increases if the two connected neurons produce output signals of the same sign. If the learning parameter is negative, and the two connected
% neurons produce output signals of the same sign, then the synaptic weight of the postsynaptic neuron, decreases in magnitude. Otherwise it increases.
% 神经元级别没有学习参数
ojas_w(neural_parameters) ->
    [];
% 权重级别的学习参数为：[H]
ojas_w(weight_parameters) ->
    [(rand:uniform() - 0.5)];
% 扰动学习参数
ojas_w({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    InputIdPs = N#neuron.input_idps,
    U_InputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
    N#neuron{input_idps = U_InputIdPs}.

%ojas_w/4 function operates on each Input_PIdP, calling the ojas_rule_w/4 function which processes each of the complementary Is and WPs lists,
% producing the Updated_WPs list in return, with the updated/adapted weights based on the oja's learning rule, using each synaptic weight's
% distinct learning parameter.
% 根据学习规则调整神经元连接权重
ojas_w(_Neural_Parameters, IAcc, Input_PIdPs, Output) ->
    ojas_w1(IAcc, Input_PIdPs, Output, []).

ojas_w1([{IPId, Is}|IAcc], [{IPId, WPs}|Input_PIdPs], Output, Acc) ->
    Updated_WPs = ojas_rule_w(Is, WPs, Output, []),
    ojas_w1(IAcc, Input_PIdPs, Output, [{IPId, Updated_WPs}|Acc]);
ojas_w1([], [], _Output, Acc) ->
    lists:reverse(Acc);
ojas_w1([], [{bias, WPs}], _Output, Acc) ->
    lists:reverse([{bias, WPs}|Acc]).

% ojas_rule_w/4 applies the ojas learning rule to each weight, using the input value I, the neuron's calculated output Output, and each weight's
% learning parameter H.
ojas_rule_w([I|Is], [{W, [H]}|WPs], Output, Acc) ->
    Updated_W = W + H * Output * (I - Output * W),
    ojas_rule_w(Is, WPs, Output, [{Updated_W, [H]}|Acc]);
ojas_rule_w([], [], _Output, Acc) ->
    lists:reverse(Acc).

%%====================================================================
%% Oja（标准版）：基于活动历史调整连接权重，Oja对权重的调整可自我稳定
%%====================================================================

% ojas/1 function produces the necessary parameter list for the oja's learning rule to operate. The parameter list for oja's learning rule is a list
% composed of a single parameter H: [H], used by the neuron for all its synaptic weights. If the learning parameter is positive, and the two connected
% neurons produce output signals of the same sign, then the postsynaptic neuron's synaptic weight increases. Otherwise it decreases.
% 神经元级别的学习参数为：[H]
ojas(neural_parameters) ->
    [(rand:uniform() - 0.5)];
% 权重级别没有学习参数
ojas(weight_parameters) ->
    [];
% 扰动学习参数
ojas({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    {PFName, ParameterList} = N#neuron.pf,
    Spread = ?SAT_LIMIT * 10, % 神经元级别的扰动幅度为10倍
    MutationProb = 1 / math:sqrt(length(ParameterList)),
    U_ParameterList = perturb(ParameterList, MutationProb, Spread, []),
    U_PF = {PFName, U_ParameterList},
    N#neuron{pf = U_PF}.

% ojas/4 function operates on each Input_PIdP, calling the ojas_rule/5 function which processes each of the complementary Is and WPs lists, producing
% the Updated_WPs list in return, with the updated/adapted weights based on the standard oja's learning rule.
% 根据学习规则调整神经元连接权重（_M是前向神经元的输出作为调制信号输入，这里无效，纯粹兼容）
ojas([_M, H], IAcc, Input_PIdPs, Output) ->
    ojas(H, IAcc, Input_PIdPs, Output, []).

ojas(H, [{IPId, Is}|IAcc], [{IPId, WPs}|Input_PIdPs], Output, Acc) ->
    Updated_WPs = ojas_rule(H, Is, WPs, Output, []),
    ojas(H, IAcc, Input_PIdPs, Output, [{IPId, Updated_WPs}|Acc]);
ojas(_H, [], [], _Output, Acc) ->
    lists:reverse(Acc);
ojas(_H, [], [{bias, WPs}], _Output, Acc) ->
    lists:reverse([{bias, WPs}|Acc]).

% ojas_rule/5 updates every synaptic weight using Oja's learning rule.
ojas_rule(H, [I|Is], [{W, []}|WPs], Output, Acc) ->
    Updated_W = W + H * Output * (I - Output * W),
    ojas_rule(H, Is, WPs, Output, [{Updated_W, []}|Acc]);
ojas_rule(_H, [], [], _Output, Acc) ->
    lists:reverse(Acc).

%%====================================================================
%% 神经调制：
%% 通过嵌入若干专门的次级调制神经元（有自己的权重和激活函数）来动态计算一部分学习参数，
%% 然后再通过学习规则对被调制的神经元的连接权重进行调整。
%% 调制神经元是在神经元同一进程内模拟的，并非单独实在的进程，有计算效率上的好处。
%%
%% 学习参数或计算学习参数的权重系数：
%% 1）神经元级别的学习参数：neuron结构里pf的ParameterList部分
%% 2）连接权重级别的学习参数的计算系数（即调制神经元的一种权重，后缀_W）：
%%    neuron结构里input_idps中每个连接权重对应的ParameterList部分
%% 3）调制神经元的另外一种权重：neuron结构里input_idps_modulation的权重部分
%% 其中，1）、2）属于自我神经调制方法，作用在被调制神经元的标准（传统）输入上，
%% 而3）属于另外一种神经调制方法，作用在调制输入上（专门区分了调制输入与标准输入）
%%====================================================================

%%====================================================================
%% 自我调制（V1）
%%====================================================================

% 神经元级别的学习参数为：[A,B,C,D]
self_modulationV1(neural_parameters) ->
    A = 0.1,
    B = 0,
    C = 0,
    D = 0,
    [A, B, C, D];
% 权重级别的学习参数为：[H]，这里实际为[H_W]，用于动态计算H
self_modulationV1(weight_parameters) ->
    [rand:uniform() - 0.5];
% 扰动学习参数：H_W（A、B、C、D写死的，不扰动）
self_modulationV1({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    InputIdPs = N#neuron.input_idps,
    U_InputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
    N#neuron{input_idps = U_InputIdPs}.

% 根据学习规则调整神经元连接权重（_M是前向神经元的输出作为调制信号输入，这里无效，纯粹兼容）
% 注意：IAcc、Input_PIdPs、Output，是来自standard，而非modulatory
self_modulationV1([_M, A, B, C, D], IAcc, Input_PIdPs, Output) ->
    H = math:tanh(dot_productV1(IAcc, Input_PIdPs)),
    neuromodulation([H, A, B, C, D], IAcc, Input_PIdPs, Output, []).

dot_productV1(IAcc, IPIdPs) ->
    dot_productV1(IAcc, IPIdPs, 0).

dot_productV1([{IPId, Input}|IAcc], [{IPId, WeightsP}|IPIdPs], Acc) ->
    Dot = dotV1(Input, WeightsP, 0),
    dot_productV1(IAcc, IPIdPs, Dot + Acc);
dot_productV1([], [{bias, [{_Bias, [H_Bias]}]}], Acc) ->
    Acc + H_Bias;
dot_productV1([], [], Acc) ->
    Acc.

dotV1([I|Input], [{_W, [H_W]}|Weights], Acc) ->
    dotV1(Input, Weights, I * H_W + Acc);
dotV1([], [], Acc) ->
    Acc.

% neuromodulation/5
neuromodulation([H, A, B, C, D], [{IPId, Is}|IAcc], [{IPId, WPs}|Input_PIdPs], Output, Acc) ->
    Updated_WPs = genheb_rule([H, A, B, C, D], Is, WPs, Output, []),
    neuromodulation([H, A, B, C, D], IAcc, Input_PIdPs, Output, [{IPId, Updated_WPs}|Acc]);
neuromodulation(_NeuralParameters, [], [], _Output, Acc) ->
    lists:reverse(Acc);
neuromodulation([H, A, B, C, D], [], [{bias, WPs}], Output, Acc) ->
    Updated_WPs = genheb_rule([H, A, B, C, D], [1], WPs, Output, []), % bias对应的输入默认是[1]
    lists:reverse([{bias, Updated_WPs}|Acc]).

% 一般化Hebb规则
% Updated_W(i) = W(i) + H * (A * I(i) * Output + B * I(i) + C * Output + D)
genheb_rule([H, A, B, C, D], [I|Is], [{W, Ps}|WPs], Output, Acc) ->
    Updated_W = functions:saturation(W + H * (A * I * Output + B * I + C * Output + D), ?SAT_LIMIT),
    genheb_rule([H, A, B, C, D], Is, WPs, Output, [{Updated_W, Ps}|Acc]);
genheb_rule(_LPs, [], [], _Output, Acc) ->
    lists:reverse(Acc).

%%====================================================================
%% 自我调制（V2）
%%====================================================================

% 神经元级别的学习参数为：[A,B,C,D]
self_modulationV2(neural_parameters) ->
    A = rand:uniform() - 0.5,
    B = 0,
    C = 0,
    D = 0,
    [A, B, C, D];
% 权重级别的学习参数为：[H]，这里实际为[H_W]，用于动态计算H
self_modulationV2(weight_parameters) ->
    [rand:uniform() - 0.5];
% 扰动学习参数：A，H_W
self_modulationV2({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    {PFName, [A|ParameterList]} = N#neuron.pf,
    [U_A] = perturb([A], 0.5, ?SAT_LIMIT * 10, []), % 扰动概率50%，神经元级别的扰动幅度为10倍
    U_PF = {PFName, [U_A|ParameterList]},
    InputIdPs = N#neuron.input_idps,
    U_InputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
    N#neuron{pf = U_PF, input_idps = U_InputIdPs}.

% 根据学习规则调整神经元连接权重（_M是前向神经元的输出作为调制信号输入，这里无效，纯粹兼容）
self_modulationV2([_M, A, B, C, D], IAcc, Input_PIdPs, Output) ->
    H = math:tanh(dot_productV1(IAcc, Input_PIdPs)),
    neuromodulation([H, A, B, C, D], IAcc, Input_PIdPs, Output, []).

%%====================================================================
%% 自我调制（V3）
%%====================================================================

% 神经元级别的学习参数为：[A,B,C,D]
self_modulationV3(neural_parameters) ->
    A = rand:uniform() - 0.5,
    B = rand:uniform() - 0.5,
    C = rand:uniform() - 0.5,
    D = rand:uniform() - 0.5,
    [A, B, C, D];
% 权重级别的学习参数为：[H]，这里实际为[H_W]，用于动态计算H
self_modulationV3(weight_parameters) ->
    [rand:uniform() - 0.5];
% 扰动学习参数：A,B,C,D，H_W
self_modulationV3({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    {PFName, ParameterList} = N#neuron.pf,
    MSpread = ?SAT_LIMIT * 10, % 神经元级别的扰动幅度为10倍
    MutationProb = 1 / math:sqrt(length(ParameterList)),
    U_ParameterList = perturb(ParameterList, MutationProb, MSpread, []),
    U_PF = {PFName, U_ParameterList},
    InputIdPs = N#neuron.input_idps,
    U_InputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
    N#neuron{pf = U_PF, input_idps = U_InputIdPs}.

% 根据学习规则调整神经元连接权重（_M是前向神经元的输出作为调制信号输入，这里无效，纯粹兼容）
self_modulationV3([_M, A, B, C, D], IAcc, Input_PIdPs, Output) ->
    H = math:tanh(dot_productV1(IAcc, Input_PIdPs)),
    neuromodulation([H, A, B, C, D], IAcc, Input_PIdPs, Output, []).

%%====================================================================
%% 自我调制（V4）
%%====================================================================

% 神经元级别的学习参数为：[B,C,D]
self_modulationV4(neural_parameters) ->
    B = 0,
    C = 0,
    D = 0,
    [B, C, D];
% 权重级别的学习参数为：[H,A]，这里实际为[H_W,A_W]，用于动态计算H,A
self_modulationV4(weight_parameters) ->
    [rand:uniform() - 0.5, rand:uniform() - 0.5];
% 扰动学习参数：H_W,A_W
self_modulationV4({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    InputIdPs = N#neuron.input_idps,
    U_InputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
    N#neuron{input_idps = U_InputIdPs}.

% 根据学习规则调整神经元连接权重（_M是前向神经元的输出作为调制信号输入，这里无效，纯粹兼容）
self_modulationV4([_M, B, C, D], IAcc, Input_PIdPs, Output) ->
    {AccH, AccA} = dot_productV4(IAcc, Input_PIdPs),
    H = math:tanh(AccH),
    A = math:tanh(AccA),
    neuromodulation([H, A, B, C, D], IAcc, Input_PIdPs, Output, []).

dot_productV4(IAcc, IPIdPs) ->
    dot_productV4(IAcc, IPIdPs, 0, 0).

dot_productV4([{IPId, Input}|IAcc], [{IPId, WeightsP}|IPIdPs], AccH, AccA) ->
    {DotH, DotA} = dotV4(Input, WeightsP, 0, 0),
    dot_productV4(IAcc, IPIdPs, DotH + AccH, DotA + AccA);
dot_productV4([], [{bias, [{_Bias, [H_Bias, A_Bias]}]}], AccH, AccA) ->
    {AccH + H_Bias, AccA + A_Bias};
dot_productV4([], [], AccH, AccA) ->
    {AccH, AccA}.

dotV4([I|Input], [{_W, [H_W, A_W]}|Weights], AccH, AccA) ->
    dotV4(Input, Weights, I * H_W + AccH, I * A_W + AccA);
dotV4([], [], AccH, AccA) ->
    {AccH, AccA}.

%%====================================================================
%% 自我调制（V5）
%%====================================================================

% 神经元级别的学习参数为：[B,C,D]
self_modulationV5(neural_parameters) ->
    B = rand:uniform() - 0.5,
    C = rand:uniform() - 0.5,
    D = rand:uniform() - 0.5,
    [B, C, D];
% 权重级别的学习参数为：[H,A]，这里实际为[H_W,A_W]，用于动态计算H,A
self_modulationV5(weight_parameters) ->
    [rand:uniform() - 0.5, rand:uniform() - 0.5];
% 扰动学习参数：B,C,D，H_W,A_W
self_modulationV5({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    {PFName, ParameterList} = N#neuron.pf,
    MSpread = ?SAT_LIMIT * 10, % 神经元级别的扰动幅度为10倍
    MutationProb = 1 / math:sqrt(length(ParameterList)),
    U_ParameterList = perturb(ParameterList, MutationProb, MSpread, []),
    U_PF = {PFName, U_ParameterList},
    InputIdPs = N#neuron.input_idps,
    U_InputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
    N#neuron{pf = U_PF, input_idps = U_InputIdPs}.

% 根据学习规则调整神经元连接权重（_M是前向神经元的输出作为调制信号输入，这里无效，纯粹兼容）
self_modulationV5([_M, B, C, D], IAcc, Input_PIdPs, Output) ->
    {AccH, AccA} = dot_productV4(IAcc, Input_PIdPs),
    H = math:tanh(AccH),
    A = math:tanh(AccA),
    neuromodulation([H, A, B, C, D], IAcc, Input_PIdPs, Output, []).

%%====================================================================
%% 自我调制（V6）
%%====================================================================

% 神经元级别没有学习参数
self_modulationV6(neural_parameters) ->
    [];
% 权重级别的学习参数为：[H,A,B,C,D]，这里实际为[H_W,A_W,B_W,C_W,D_W]，用于动态计算H,A,B,C,D
self_modulationV6(weight_parameters) ->
    H_W = rand:uniform() - 0.5,
    A_W = rand:uniform() - 0.5,
    B_W = rand:uniform() - 0.5,
    C_W = rand:uniform() - 0.5,
    D_W = rand:uniform() - 0.5,
    [H_W, A_W, B_W, C_W, D_W];
% 扰动学习参数：H_W,A_W,B_W,C_W,D_W
self_modulationV6({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    InputIdPs = N#neuron.input_idps,
    U_InputIdPs = perturb_parameters(InputIdPs, ?SAT_LIMIT),
    N#neuron{input_idps = U_InputIdPs}.

% 根据学习规则调整神经元连接权重（_M是前向神经元的输出作为调制信号输入，这里无效，纯粹兼容）
self_modulationV6([_M, B, C, D], IAcc, Input_PIdPs, Output) ->
    {AccH, AccA, AccB, AccC, AccD} = dot_productV6(IAcc, Input_PIdPs),
    H = math:tanh(AccH),
    A = math:tanh(AccA),
    B = math:tanh(AccB),
    C = math:tanh(AccC),
    D = math:tanh(AccD),
    neuromodulation([H, A, B, C, D], IAcc, Input_PIdPs, Output, []).

dot_productV6(IAcc, IPIdPs) ->
    dot_productV6(IAcc, IPIdPs, 0, 0, 0, 0, 0).

dot_productV6([{IPId, Input}|IAcc], [{IPId, WeightsP}|IPIdPs], AccH, AccA, AccB, AccC, AccD) ->
    {DotH, DotA, DotB, DotC, DotD} = dotV6(Input, WeightsP, 0, 0, 0, 0, 0),
    % 点积值的累加
    dot_productV6(IAcc, IPIdPs, DotH + AccH, DotA + AccA, DotB + AccB, DotC + AccC, DotD + AccD);
dot_productV6([], [{bias, [{_Bias, [H_Bias, A_Bias, B_Bias, C_Bias, D_Bias]}]}], AccH, AccA, AccB, AccC, AccD) ->
    {AccH + H_Bias, AccA + A_Bias, AccB + B_Bias, AccC + C_Bias, AccD + D_Bias};
dot_productV6([], [], AccH, AccA, AccB, AccC, AccD) ->
    {AccH, AccA, AccB, AccC, AccD}.

% 对于每一个输入Id对应的输入向量Input，把向量的每一维元素值I分别乘以H_W,A_W,B_W,C_W,D_W然后各自累加
dotV6([I|Input], [{_W, [H_W, A_W, B_W, C_W, D_W]}|Weights], AccH, AccA, AccB, AccC, AccD) ->
    dotV6(Input, Weights, I * H_W + AccH, I * A_W + AccA, I * B_W + AccB, I * C_W + AccC, I * D_W + AccD);
dotV6([], [], AccH, AccA, AccB, AccC, AccD) ->
    {AccH, AccA, AccB, AccC, AccD}.

%%====================================================================
%% 神经调制：作用在调制输入上（专门区分了调制输入与标准输入）
%% neuromodulation/4的参数M：即为调制输入处理后的输出，
%% 再连同学习参数、标准输入、输出、连接权重一起通过一般Hebb规则处理一遍，最终得到调制后的连接权重
%%====================================================================

% 神经元级别的学习参数为：[H,A,B,C,D]
neuromodulation(neural_parameters) ->
    H = rand:uniform() - 0.5,
    A = rand:uniform() - 0.5,
    B = rand:uniform() - 0.5,
    C = rand:uniform() - 0.5,
    D = rand:uniform() - 0.5,
    [H, A, B, C, D];
% 权重级别没有学习参数
neuromodulation(weight_parameters) ->
    [];
% 扰动学习参数：H,A,B,C,D
neuromodulation({N_Id, mutate}) ->
    rand:seed(exs64, util:now()),
    N = genotype:read({neuron, N_Id}),
    {PFName, ParameterList} = N#neuron.pf,
    MSpread = ?SAT_LIMIT * 10, % 神经元级别的扰动幅度为10倍
    MutationProb = 1 / math:sqrt(length(ParameterList)),
    U_ParameterList = perturb(ParameterList, MutationProb, MSpread, []),
    U_PF = {PFName, U_ParameterList},
    N#neuron{pf = U_PF}.

% 根据学习规则调整神经元连接权重（M是前向神经元的输出作为调制信号输入）
% neuromodulation/4
neuromodulation([M, H, A, B, C, D], IAcc, Input_PIdPs, Output) ->
    Modulator = functions:scale_dzone(M, 0.33, ?SAT_LIMIT), % 将[0.33, 2Pi]映射到[0, 2Pi]后，M的值
    % 当-0.33 =< M =< 0.33时，Modulator=0，从而H'=0，权重不会更新
    neuromodulation([Modulator * H, A, B, C, D], IAcc, Input_PIdPs, Output, []).

