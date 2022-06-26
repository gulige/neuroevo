%% 基底是一整块对称的由神经节点组成的超立方体结构，每一个神经节点（neurode）对应一个位置坐标（也只承载一个数值），通过一个NN来负责设置neurode之间的权重
%% 基底由超层（hyperlayer）组成：输入超层、若干隐藏超层、输出超层，超层由若干超平面（hyperplane）组成，输入（出）超层的一个超平面就是一个传感器（执行器）
%% 带有基底的工作流：
%% 1. exoself派生出神经元、传感器、执行器、基底cpp、基底cep、基底，以及cortex等进程；
%% 2. cortex发送sync消息到所有的传感器，呼唤它们开始行动；
%% 3. 传感器从环境中获取到感知信号；
%% 4. 传感器对感知信号做后处理；
%% 5. 传感器把处理后的感知信号发送给基底进程；
%% 6. 基底进程从所有传感器收集感知信号，并基于这些信号、基底密度、执行器和link_form来构建一个基底结构（当基底状态标识为reset时，否则若标识为hold则跳到下一步）；
%% 7. 基底进程给相连的cpp发送一对相连神经节点的坐标；
%% 8. cpp处理坐标对，并生成一个新的坐标向量；
%% 9. cpp将处理过的坐标向量传递给其所相连的NN中的神经元；
%% 10. NN处理坐标信号；
%% 11. NN输出层中的神经元产生输出信号，并发送到其相连的cep；
%% 12. cep等待并收集到它所相连的所有神经元的输出信号，然后处理整个积累完成的信号；
%% 13. cep把处理完的向量信号传递给基底进程（用作前面给定神经节点间的连接权重）；
%% 14. 基底进程为基底中每一对相连的神经节点呼唤cpp（步骤7），一旦所有的神经节点都拥有了它们各自的连接权重，基底进程将把来自传感器的信号映射到基底的输入超层，
%%     然后隐藏超层对传感器信号进行层层加权处理，直到稍后在输出超层中产生了输出信号；
%% 15. 输出超层的每一个超平面关联到一个执行器，每个超平面的输出向量将传递给相应的执行器；
%% 16. 执行器收集其扇入列表（即fanin_ids，这里只有基底进程）中所有信号；
%% 17. 执行器使用这些信号与环境交互；
%% 18. 执行器发送回sync消息给cortex；
%% 19. cortex从所有的执行器收集回所有的sync消息；
%% 20. cortex再次呼唤传感器，开始新一轮的Sense-Think-Act循环（跳回步骤3）。

-module(substrate).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

-define(SAT_LIMIT, 3.14159).

-record(state, {
    type,
    plasticity = none, % [none, iterative, abcn]
    morphology,
    specie_id,
    sensors,
    actuators,
    spids = [], % sensor pids
    apids = [], % actuator pids
    cpp_pids = [], % coordinate pre-processor pid list
    cep_pids = [], % connectivity expression producer pid list
    densities, % [隐藏（非输入、非输出）超层的深度（即有多少个隐藏超层）, 超层第N维的密度（即含了多少个超平面）, 超层第N-1维（超平面的最后一维）的密度, ...]
    substrate_state_flag, % [reset, hold, iterative]，每当NN的权重扰动时需要reset
    old_substrate, % 备份的基底
    cur_substrate, % 在用的基底
    % 基底格式：[输入超层, 隐藏超层1...隐藏超层N, 输出超层]，超层为神经节点元组{坐标, 输出, 输入权重列表}的列表（结构层次上并不体现超平面，因为坐标已经能表明它）
    link_form % [l2l_feedforward, fully_interconnected, jordan_recurrent, neuronself_recurrent, freeform]
}).

gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf) ->
    rand:seed(exs64, util:now()),
    receive
        {ExoSelf, init, InitState} ->
            {Sensors, Actuators, SPIds, APIds, CPP_PIds, CEP_PIds, Densities, Plasticity, LinkForm} = InitState,
            ?DBG("Substrate InitState:~p~n", [InitState]),
            S = #state{
                sensors = Sensors,
                actuators = Actuators,
                spids = SPIds,
                apids = APIds,
                cpp_pids = CPP_PIds,
                cep_pids = CEP_PIds,
                densities = Densities,
                substrate_state_flag = reset, % 初始设成重置
                old_substrate = void,
                cur_substrate = init, % 在处理（Thinking）阶段时创建（先收集传感信号）
                plasticity = Plasticity,
                link_form = LinkForm
            },
            substrate:loop(ExoSelf, S, SPIds, [])
    end.

loop(ExoSelf, S, [SPId|SPIds], SAcc) ->
    receive
        {SPId, forward, Sensory_Signal} ->
            loop(ExoSelf, S, SPIds, [Sensory_Signal|SAcc]);
        {ExoSelf, reset_substrate} ->
            U_S = S#state{
                old_substrate = S#state.cur_substrate,
                substrate_state_flag = reset
            },
            ExoSelf ! {self(), ready},
            loop(ExoSelf, U_S, [SPId|SPIds], SAcc);
        {ExoSelf, backup_substrate} ->
            ?DBG("Substrate resetting:~n"),
            U_S = S#state{
                old_substrate = S#state.cur_substrate,
                substrate_state_flag = hold
            },
            ExoSelf ! {self(), ready},
            loop(ExoSelf, U_S, [SPId|SPIds], SAcc);
        {ExoSelf, revert_substrate} ->
            ?DBG("Substrate reverting:~n"),
            U_S = S#state{
                cur_substrate = S#state.old_substrate,
                substrate_state_flag = hold
            },
            ExoSelf ! {self(), ready},
            loop(ExoSelf, U_S, [SPId|SPIds], SAcc);
        {ExoSelf, terminate} ->
            ?INFO("********Substrate terminate:~p~n", [S]),
            void;
        Msg ->
            ?INFO("Substrate Unknown Msg:~p~n", [Msg]),
            loop(ExoSelf, S, [SPId|SPIds], SAcc)
        %after 20000 ->
        %    ?ERR("********Substrate stuck:~p~n", [S])
    end;
loop(ExoSelf, S, [], SAcc) -> % all sensory signals received
    {U_Substrate, U_SMode, OAcc} = reason(SAcc, S),
    advanced_fanout(OAcc, S#state.actuators, S#state.apids),
    U_S = S#state{
        cur_substrate = U_Substrate,
        substrate_state_flag = U_SMode
    },
    loop(ExoSelf, U_S, S#state.spids, []).

% 推理（Thinking阶段）
reason(Input, S) ->
    Densities = S#state.densities,
    Substrate = S#state.cur_substrate,
    SMode = S#state.substrate_state_flag,
    CPP_PIds = S#state.cpp_pids,
    CEP_PIds = S#state.cep_pids,
    Plasticity = S#state.plasticity,
    LinkForm = S#state.link_form,
    case SMode of
        reset ->
            Sensors = S#state.sensors,
            Actuators = S#state.actuators,
            New_Substrate = create_substrate(Sensors, Densities, Actuators, LinkForm),
            U_SMode = case Plasticity of
                iterative ->
                    {Output, Populated_Substrate} =
                        calculate_ResetOutput(Densities, New_Substrate, Input, CPP_PIds, CEP_PIds, Plasticity, LinkForm),
                    iterative;
                _ -> % none, abcn
                    {Output, Populated_Substrate} =
                        calculate_ResetOutput(Densities, New_Substrate, Input, CPP_PIds, CEP_PIds, Plasticity, LinkForm),
                    hold
            end,
            {Populated_Substrate, U_SMode, Output};
        iterative ->
            {Output, U_Substrate} =
                calculate_ResetOutput(Densities, Substrate, Input, CPP_PIds, CEP_PIds, Plasticity, LinkForm),
            {U_Substrate, SMode, Output};
        hold ->
            {Output, U_Substrate} =
                calculate_HoldOutput(Densities, Substrate, Input, CPP_PIds, CEP_PIds, Plasticity, LinkForm),
            {U_Substrate, SMode, Output}
    end.

advanced_fanout(OAcc, [Actuator|Actuators], [APId|APIds]) ->
    {Output, OAccRem} = lists:split(Actuator#actuator.vl, OAcc),
    APId ! {self(), forward, Output},
    advanced_fanout(OAccRem, Actuators, APIds);
advanced_fanout([], [], []) ->
    done.

%%====================================================================
%% Internal functions
%%====================================================================

flush_buffer() ->
    receive
        Any -> %?DBG("Any:~p~n", [Any]),
            flush_buffer()
    after 0 ->
        done
end.

% format选项：
% no_geo
% {symmetric, [R1,R2...Rk], [Val1...Valn]} where n == R1*R2*...Rk and k = dimension.
% {asymmetric, [[R1...Rp], [R1...Rt]], [Val1...Valn]} where n == lists:sum(lists:flatten([[R1...Rp], [R1...Rt]])), and depth = dimension.
% coorded, every val comes with its own coord tuple: {Coord, Val}. The coord is a list, thus specifying the dimensionality.
test_cs() ->
    Sensors = [
        #sensor{format = no_geo, vl = 3},
        #sensor{format = {symmetric, lists:reverse([2, 3])}, vl = 6}
    ],
    Actuators = [
        #actuator{format = no_geo, vl = 2},
        #actuator{format = {symmetric, lists:reverse([3, 2])}, vl = 6}
    ],
    create_substrate(Sensors, [3, 2, 3, 2], Actuators, l2l_feedforward).

test_IS(SubstrateDimension) ->
    Sensors = [
        #sensor{format = no_geo, vl = 10},
        #sensor{format = {symmetric, lists:reverse([3, 4])},
                vl = [1,-1,-1,-1,
                      1,-1,-1,-1,
                      1,1,1,1]}
    ],
    compose_ISubstrate(Sensors, SubstrateDimension).

test_OS(SubstrateDimension) ->
    Actuators = [
        #actuator{format = no_geo, vl = 10},
        #actuator{format = {symmetric, lists:reverse([3, 4])},
                  vl = [1,-1,-1,-1,
                        1,-1,-1,-1,
                        1,1,1,1]}
    ],
    compose_OSubstrate(Actuators, SubstrateDimension, [w1, w2, w3]).

%%--------------------------------------------------------------------
%% 构建
%%--------------------------------------------------------------------

 % 返回基底结构：[输入超层, 隐藏超层1...隐藏超层N, 输出超层]，超层为神经节点元组{坐标, 输出, 输入权重列表}的列表（结构层次上并不体现超平面，因为坐标已经能表明它）
create_substrate(Sensors, Densities, Actuators, LinkForm) ->
    [HiddenDepth|SubDensities] = Densities, % Densities（K,Z,Y,X）是隐藏超层的
    % I：Input，O：Output，H：Hidden
    Substrate_I = compose_ISubstrate(Sensors, length(Densities)), % 构造输入层，返回一维向量
    I_VL = length(Substrate_I),
    case LinkForm of
        l2l_feedforward -> % 分层前向传递
            Weight = 0,
            H = mult(SubDensities), % Z * Y * X，单个隐藏超层（含Z个超平面）中神经节点的总个数
            % 输入层只是传感器的映射（不处理数据），它代表着传感器，因此其自身是没有输入的，也就没有连接权重
            IWeights = lists:duplicate(I_VL, Weight), % 紧邻输入层的隐藏层的权重列表
            HWeights = lists:duplicate(H, Weight); % 其他后续层（包括输出层）的权重列表
        fully_interconnected -> % 全连接
            Output_Neurodes = tot_ONeurodes(Actuators, 0),
            Weight = 0,
            Tot_HiddenNeurodes = mult(Densities),
            Tot_Weights = Tot_HiddenNeurodes + I_VL + Output_Neurodes, % 因为全连接，所以单个神经节点的权重数是整个基底中所有神经节点的总个数（包括它自己）
            IWeights = lists:duplicate(Tot_Weights, Weight), % 紧邻输入层的隐藏层的权重列表
            HWeights = lists:duplicate(Tot_Weights, Weight); % 其他后续层（包括输出层）的权重列表
        jordan_recurrent -> % 约旦循环（在分层前向传递的基础上，第1个隐藏层的输入除了原本的输入层外，还包括输出层也作为其输入）
            Output_Neurodes = tot_ONeurodes(Actuators, 0),
            Weight = 0,
            H = mult(SubDensities), % Z * Y * X，单个隐藏超层（含Z个超平面）中神经节点的总个数
            IWeights = lists:duplicate(I_VL + Output_Neurodes, Weight), % 紧邻输入层的隐藏层（第1个隐藏层）的权重列表，输出层也作为其输入
            HWeights = lists:duplicate(H, Weight); % 其他后续层（包括输出层）的权重列表
        neuronself_recurrent -> % 神经元自循环（在分层前向传递的基础上，单个神经节点的输入还包括其自身的输出）
            Weight = 0,
            H = mult(SubDensities), % Z * Y * X，单个隐藏超层（含Z个超平面）中神经节点的总个数
            IWeights = lists:duplicate(I_VL + 1, Weight), % 紧邻输入层的隐藏层的权重列表
            HWeights = lists:duplicate(H + 1, Weight) % 其他后续层（包括输出层）的权重列表
    end,
    case HiddenDepth of
        0 -> % 没有隐藏层
            Substrate_O = compose_OSubstrate(Actuators, length(Densities), IWeights),
            [Substrate_I, Substrate_O];
        1 -> % 1个隐藏层
            % cs: create substrate
            Substrate_R = cs(SubDensities, IWeights), % R: 紧挨着输入层的第一个隐藏层
            Substrate_O = compose_OSubstrate(Actuators, length(Densities), HWeights),
            [Substrate_I, extrude(0, Substrate_R), Substrate_O]; % extrude：在坐标中挤入（往上扩展）维度K的坐标分量，因为这里隐藏层只有1层，所以K坐标分量必然为0
        _ -> % >= 2
            Substrate_R = cs(SubDensities, IWeights), % R: 紧挨着输入层的第一个隐藏层
            Substrate_H = cs(SubDensities, HWeights),
            Substrate_O = compose_OSubstrate(Actuators, length(Densities), HWeights),
            [_, RCoord|C1] = build_CoordList(HiddenDepth + 2), % 2即包含输入层、输出层坐标，_对应输入层，即掐头
            [_|C2] = lists:reverse(C1), % _对应输出层，即去尾
            HCoords = lists:reverse(C2), % 掐头去尾剩下的是各隐藏层（除掉第一个隐藏层）的维度K的坐标分量列表
            ESubstrate_R = extrude(RCoord, Substrate_R), % 在第一个隐藏层内神经节点的坐标中挤入维度K的坐标分量
            ESubstrates_H = [extrude(HCoord, Substrate_H) || HCoord <- HCoords], % 在其他各隐藏层内神经节点的坐标中挤入维度K的坐标分量
            % 这里，在得到K坐标分量前其他各隐藏层是一样的内容（神经节点的输出、权重都是0），所以上面都用Substrate_H，在为其挤入不同的K坐标分量后，最终得到一个列表Substrates_H
            lists:append([[Substrate_I, ESubstrate_R], ESubstrates_H, [Substrate_O]])
    end.

% 构造输入层，返回一维向量，SubstrateDimension是隐藏超层的总维数
compose_ISubstrate(Sensors, SubstrateDimension) ->
    % 减去2维：K（超层堆叠的维度，也叫Lead维度）和Z（超平面堆叠的维度，也叫Depth维度）
    compose_ISubstrate(Sensors, [], 1, SubstrateDimension - 2).

% part：对应着一个超平面
compose_ISubstrate([S|Sensors], Acc, Max_Dim, Required_Dim) ->
    case S#sensor.format of
        undefined ->
            Dim = 1,
            CoordLists = create_CoordLists([S#sensor.vl]), % 构造超平面内坐标列表（CoordLists），而坐标本身也是一个列表（即CoordList）
            ISubstrate_Part = [{Coord, 0, void} || Coord <- CoordLists], % void：输入层权重没有意义
            {Dim, ISubstrate_Part};
        no_geo ->
            Dim = 1,
            CoordLists = create_CoordLists([S#sensor.vl]),
            ISubstrate_Part = [{Coord, 0, void} || Coord <- CoordLists],
            {Dim, ISubstrate_Part};
        {symmetric, Resolutions} -> % Resolutions：[超平面第N维的密度, 超平面第N-1维的密度, ...]
            Dim = length(Resolutions),
            Signal_Length = mult(Resolutions),
            CoordLists = create_CoordLists(Resolutions),
            ISubstrate_Part = [{Coord, 0, void} || Coord <- CoordLists],
            {Dim, ISubstrate_Part};
        {coorded, Dim, Resolutions, ISubstrate_Part} -> % 已经坐标化了
            {Dim, ISubstrate_Part}
    end,
    U_Dim = max(Max_Dim, Dim), % 跟踪记录所有传感器里维数最大的
    compose_ISubstrate(Sensors, [ISubstrate_Part|Acc], U_Dim, Required_Dim);
compose_ISubstrate([], Acc, ISubstratePart_MaxDim, Required_Dim) ->
    % Required_Dim是隐藏层超平面本身的维数，它应该要不小于输入层任何超平面本身的维数
    case Required_Dim >= ISubstratePart_MaxDim of
        true ->
            ISubstrate_Depth = length(Acc), % 等于传感器个数
            ISubstrate_DepthCoords = build_CoordList(ISubstrate_Depth), % 构造输入层维度Z的坐标分量列表
            adv_extrude(Acc, Required_Dim, lists:reverse(ISubstrate_DepthCoords), -1, []); % -1：输入层维度K的坐标分量
            % Passed in inverted, reversed inside adv_extrude, same for depth coords.
        false -> % 隐藏层超平面本身的维数少了，低维是无法处理高维的！
            exit("Error in compose_ISubstrate, Required_Dim < ISubstratePart_MaxDim~n")
    end.

% 返回挤压好坐标的输入超层（已经有O、W及超平面内的坐标，加入Depth和Lead坐标分量）
% 也可用于输出超层
adv_extrude([ISubstrate_Part|ISubstrate], Required_Dim, [IDepthCoord|ISubstrate_DepthCoords], LeadCoord, Acc) ->
    % ISP：Input Substrate Part，对应基底输入层的一个超平面
    % 加入Depth（Z）和Lead（K）坐标分量，并且对于输入层超平面维数少于隐藏层超平面维数的那部分，在输入层相应坐标分量上补0对齐
    Extruded_ISP =
        [{[LeadCoord, IDepthCoord|lists:append(lists:duplicate(Required_Dim - length(Coord), 0), Coord)], O, W} || {Coord, O, W} <- ISubstrate_Part],
    adv_extrude(ISubstrate, Required_Dim, ISubstrate_DepthCoords, LeadCoord, lists:append(Extruded_ISP, Acc)); % lists:append打破了超平面的边界，内部元组合一起了
adv_extrude([], _Required_Dim, [], _LeadCoord, Acc) ->
    Acc.

% 构造输出层，返回一维向量，SubstrateDimension是隐藏超层的总维数
compose_OSubstrate(Actuators, SubstrateDimension, Weights) ->
    % 减去2维：K（超层堆叠的维度，也叫Lead维度）和Z（超平面堆叠的维度，也叫Depth维度）
    compose_OSubstrate(Actuators, [], 1, SubstrateDimension - 2, Weights).

% part：对应着一个超平面
compose_OSubstrate([A|Actuators], Acc, Max_Dim, Required_Dim, Weights) ->
    case A#actuator.format of
        undefined ->
            Dim = 1,
            CoordLists = create_CoordLists([A#actuator.vl]), % 构造超平面内坐标列表（CoordLists），而坐标本身也是一个列表（即CoordList）
            OSubstrate_Part = [{Coord, 0, Weights} || Coord <- CoordLists],
            {Dim, OSubstrate_Part};
        no_geo ->
            Dim = 1,
            CoordLists = create_CoordLists([A#actuator.vl]),
            OSubstrate_Part = [{Coord, 0, Weights} || Coord <- CoordLists],
            {Dim, OSubstrate_Part};
        {symmetric, Resolutions} -> % Resolutions：[超平面第N维的密度, 超平面第N-1维的密度, ...]
            Dim = length(Resolutions),
            Signal_Length = mult(Resolutions),
            CoordLists = create_CoordLists(Resolutions),
            OSubstrate_Part = [{Coord, 0, Weights} || Coord <- CoordLists],
            {Dim, OSubstrate_Part};
        {coorded, Dim, Resolutions, Unadjusted_OSubstrate_Part} -> % 已经坐标化了
            OSubstrate_Part = [{Coord, O, Weights} || {Coord, O, _} <- Unadjusted_OSubstrate_Part],
            {Dim, OSubstrate_Part}
    end,
    U_Dim = max(Max_Dim, Dim), % 跟踪记录所有执行器里维数最大的
    compose_OSubstrate(Actuators, [OSubstrate_Part|Acc], U_Dim, Required_Dim, Weights);
compose_OSubstrate([], Acc, OSubstratePart_MaxDim, Required_Dim, _Weights) ->
    % Required_Dim是隐藏层超平面本身的维数，它应该要不小于输出层任何超平面本身的维数
    case Required_Dim >= OSubstratePart_MaxDim of
        true ->
            OSubstrate_Depth = length(Acc),
            OSubstrate_DepthCoords = build_CoordList(OSubstrate_Depth),
            adv_extrude(Acc, Required_Dim, lists:reverse(OSubstrate_DepthCoords), 1, []); % 1：输出层维度K的坐标分量
            % Passed in inverted, reversed inside adv_extrude, same for depth coords.
        false -> % 隐藏层超平面本身的维数少了，低维是无法输出高维的！
            exit("Error in compose_OSubstrate, Required_Dim < OSubstratePart_MaxDim~n")
    end.

% 构建一维坐标，在[-1,1]区间上获得Density个等距的坐标（其实是最终坐标的分量）列表
build_CoordList(Density) ->
    case Density == 1 of
        true -> % 就1个坐标的话，在零点
            [0.0];
        false ->
            DensityDividers = Density - 1, % Density个坐标分成Density-1等份
            Resolution = 2 / DensityDividers, % 每一等份的距离
            % 从右（坐标1）往左（坐标-1）构建坐标列表，DensityDividers用于countdown迭代计数
            build_CoordList(Resolution, DensityDividers, 1, [])
    end.

build_CoordList(Resolution, 0, Coord, Acc) ->
    [-1|Acc]; % 迭代到了最左边-1
build_CoordList(Resolution, DensityDividers, Coord, Acc) ->
    build_CoordList(Resolution, DensityDividers - 1, Coord - Resolution, [Coord|Acc]).

% 根据Densities创建多维的坐标，返回坐标列表（坐标本身也是一个列表，即CoordList，由多维度的坐标分量组成）
create_CoordLists(Densities) ->
    create_CoordLists(Densities, []).

create_CoordLists([Density|RDensities], []) -> % []意味着第一轮迭代，RDensities：Reversed Densities
    CoordList = build_CoordList(Density),
    XtendedCoordList = [[Coord] || Coord <- CoordList], % 将一维坐标列表的每个值扩展成列表，因为每个最终坐标本身就是一个列表
    create_CoordLists(RDensities, XtendedCoordList);
create_CoordLists([Density|RDensities], Acc) ->
    CoordList = build_CoordList(Density),
    XtendedCoordList = [[Coord|Sub_Coord] || Coord <- CoordList, Sub_Coord <- Acc],
    % Coord是待扩展的新维度的坐标分量值，Sub_Coord是当前已构建的若干维度的坐标（它是一个列表）
    % CoordList：M，Acc：N，M x N，得到XtendedCoordList
    create_CoordLists(RDensities, XtendedCoordList);
create_CoordLists([], Acc) ->
    Acc.

mult(List) ->
    mult(List, 1).

mult([Val|List], Acc) ->
    mult(List, Val * Acc);
mult([], Acc) ->
    Acc.

tot_ONeurodes([A|Actuators], Acc) ->
    Tot_ANeurodes = case A#actuator.format of
        undefined ->
            A#actuator.vl;
        no_geo ->
            A#actuator.vl;
        {symmetric, Resolutions} ->
            mult(Resolutions);
        {coorded, Dim, Resolutions, Unadjusted_OSubstrate_Part} ->
            length(Unadjusted_OSubstrate_Part)
    end,
    tot_ONeurodes(Actuators, Tot_ANeurodes + Acc);
tot_ONeurodes([], Acc) ->
    Acc.

% create substrate：针对非I、O，只限R、H，即只对隐藏超层
% Densities：实际传的是SubDensities（Z,Y,X）
% 创建一个隐藏超层，坐标只差K维（Lead）分量，含O、W，返回：[{[Z,Y,X], Output, Weights}, ...]
cs(Densities, Weights) ->
    RDensities = lists:reverse(Densities), % R：Reversed
    Substrate = create_CoordLists(RDensities, []),
    attach_ow(Substrate, 0, Weights). % 附加Output（=0）、Weights

% 在坐标列表里附加O、W
attach_ow(Substrate, O, W) ->
    attach_ow(Substrate, O, W, []).

attach_ow([Coord|Substrate], O, W, Acc) ->
    attach_ow(Substrate, O, W, [{Coord, O, W}|Acc]);
attach_ow([], _O, _W, Acc) ->
    lists:reverse(Acc).

% 在Substrate中扩展新的维度（挤入新维度的坐标分量）
extrude(NewDimension_Coord, Substrate) ->
    extrude(NewDimension_Coord, Substrate, []).

extrude(NewDimension_Coord, [{Coord, O, W}|Substrate], Acc) ->
    extrude(NewDimension_Coord, Substrate, [{[NewDimension_Coord|Coord], O, W}|Acc]);
extrude(_NewDimension_Coord, [], Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% 基底运作（种植基底权重、计算基底输出）
%%--------------------------------------------------------------------

% hold状态时，计算基底输出
% 返回：{Output, NewSubstrate}
calculate_HoldOutput(Densities, Substrate, Input, CPP_PIds, CEP_PIds, Plasticity, LinkForm) ->
    [IHyperlayer|Populated_PHyperlayers] = Substrate,
    Populated_IHyperlayer = populate_InputHyperlayer(IHyperlayer, lists:flatten(Input), []),
    {Output, U_PHyperlayers} =
        calculate_substrate_output(Populated_IHyperlayer, Populated_PHyperlayers, LinkForm, Plasticity, CPP_PIds, CEP_PIds),
    {Output, [IHyperlayer|U_PHyperlayers]}.

% reset状态时，计算基底输出
calculate_ResetOutput(Densities, Substrate, Input, CPP_PIds, CEP_PIds, Plasticity, LinkForm) ->
    [IHyperlayer|PHyperlayers] = Substrate,
    %?DBG("IHyperlayer:~p~nPHyperlayers:~p~n", [IHyperlayer, PHyperlayers]),
    Populated_IHyperlayer = populate_InputHyperlayer(IHyperlayer, lists:flatten(Input), []),
    case Plasticity of
        iterative ->
            % iterative可塑性模式，不预先为基底的处理层种植权重（缺省为0），而是在每次基底产生输出后，通过NN而非直接的学习函数来计算更新基底权重（增量方式）
            {Output, U_PHyperlayers} =
                calculate_substrate_output(Populated_IHyperlayer, PHyperlayers, LinkForm, Plasticity, CPP_PIds, CEP_PIds),
            {Output, [IHyperlayer|U_PHyperlayers]};
        _ -> % none, abcn
            Populated_PHyperlayers = populate_PHyperlayers(Substrate, CPP_PIds, CEP_PIds, LinkForm, Plasticity),
            {Output, U_PHyperlayers} =
                calculate_substrate_output(Populated_IHyperlayer, Populated_PHyperlayers, LinkForm, Plasticity, CPP_PIds, CEP_PIds),
            {Output, [IHyperlayer|U_PHyperlayers]}
    end.

%%--------------------------------------------------------------------
%% 种植基底权重
%%--------------------------------------------------------------------

% 为输入超层种植（映射）输入值
populate_InputHyperlayer([{Coord, PrevO, void}|IHyperlayer], [I|Input], Acc) ->
    populate_InputHyperlayer(IHyperlayer, Input, [{Coord, I, void}|Acc]);
populate_InputHyperlayer([], [], Acc) ->
    lists:reverse(Acc).

% 为处理超层种植权重
% PHyperlayers：processing hyperlayers（包括隐藏层和输出层）
populate_PHyperlayers(Substrate, CPP_PIds, CEP_PIds, LinkForm, Plasticity) ->
    case LinkForm of
        l2l_feedforward ->
            [IHyperlayer, PHyperlayer|RemSubstrate] = Substrate,
            populate_PHyperlayers_l2l(IHyperlayer, PHyperlayer, RemSubstrate, CPP_PIds, CEP_PIds, Plasticity, [], []);
        fully_interconnected ->
            [_IHyperlayer, PHyperlayer|RemSubstrate] = Substrate,
            I_Neurodes = lists:flatten(Substrate), % 压破超层边界，所有神经节点合在一个列表中
            populate_PHyperlayers_fi(I_Neurodes, PHyperlayer, RemSubstrate, CPP_PIds, CEP_PIds, Plasticity, [], []);
        jordan_recurrent ->
            [IHyperlayer, PHyperlayer|RemSubstrate] = Substrate,
            [OHyperlayer|_] = lists:reverse(Substrate),
            I_Neurodes = lists:flatten([IHyperlayer, OHyperlayer]), % 输入层和输出层神经节点合在一个列表中，作为输入
            populate_PHyperlayers_l2l(I_Neurodes, PHyperlayer, RemSubstrate, CPP_PIds, CEP_PIds, Plasticity, [], []);
        neuronself_recurrent ->
            [IHyperlayer, PHyperlayer|RemSubstrate] = Substrate,
            populate_PHyperlayers_nsr(IHyperlayer, PHyperlayer, RemSubstrate, CPP_PIds, CEP_PIds, Plasticity, [], [])
    end.

% l2l_feedforward
% Acc1：用于积累单层，Acc2：用于积累所有积累好的单层
populate_PHyperlayers_l2l(PrevHyperlayer, [{Coord, OldO, OldWeights}|CurHyperlayer], Substrate, CPP_PIds, CEP_PIds, Plasticity, Acc1, Acc2) ->
    NewWeights = case Plasticity of
        none ->
            % 对于当前超层中的一个神经节点坐标，建立上一超层中每个神经节点和它的连接权重
            get_weights(PrevHyperlayer, Coord, CPP_PIds, CEP_PIds, []);
        _ -> % iterative, abcn
            get_weights(PrevHyperlayer, Coord, CPP_PIds, CEP_PIds, [], OldWeights, OldO)
    end,
    populate_PHyperlayers_l2l(PrevHyperlayer, CurHyperlayer, Substrate, CPP_PIds, CEP_PIds, Plasticity, [{Coord, OldO, NewWeights}|Acc1], Acc2);
% 处理完一层时
populate_PHyperlayers_l2l(_PrevHyperlayer, [], [CurHyperlayer|Substrate], CPP_PIds, CEP_PIds, Plasticity, Acc1, Acc2) ->
    PrevHyperlayer = lists:reverse(Acc1), % 处理完的当前层成为上一层，而下一层成为当前层
    populate_PHyperlayers_l2l(PrevHyperlayer, CurHyperlayer, Substrate, CPP_PIds, CEP_PIds, Plasticity, [], [PrevHyperlayer|Acc2]);
% 处理完所有层时，返回种植好权重的Substrate
populate_PHyperlayers_l2l(_PrevHyperlayer, [], [], CPP_PIds, CEP_PIds, Plasticity, Acc1, Acc2) ->
    lists:reverse([lists:reverse(Acc1)|Acc2]).

% fully_interconnected
% FlatSubstrate：整个扁平化的Substrate，破除了超层边界，即一个神经节点列表
populate_PHyperlayers_fi(FlatSubstrate, [{Coord, OldO, OldWeights}|CurHyperlayer], Substrate, CPP_PIds, CEP_PIds, Plasticity, Acc1, Acc2) ->
    NewWeights = case Plasticity of
        none ->
            % 全连接，所以I_Neurodes不变，为所有的神经节点（包括它自己）
            get_weights(FlatSubstrate, Coord, CPP_PIds, CEP_PIds, []);
        _ -> % iterative, abcn
            get_weights(FlatSubstrate, Coord, CPP_PIds, CEP_PIds, [], OldWeights, OldO)
    end,
    populate_PHyperlayers_fi(FlatSubstrate, CurHyperlayer, Substrate, CPP_PIds, CEP_PIds, Plasticity, [{Coord, OldO, NewWeights}|Acc1], Acc2);
% 处理完一层时
populate_PHyperlayers_fi(FlatSubstrate, [], [CurHyperlayer|Substrate], CPP_PIds, CEP_PIds, Plasticity, Acc1, Acc2) ->
    populate_PHyperlayers_fi(FlatSubstrate, CurHyperlayer, Substrate, CPP_PIds, CEP_PIds, Plasticity, [], [lists:reverse(Acc1)|Acc2]);
% 处理完所有层时，返回种植好权重的Substrate
populate_PHyperlayers_fi(_FlatSubstrate, [], [], CPP_PIds, CEP_PIds, Plasticity, Acc1, Acc2) ->
    lists:reverse([lists:reverse(Acc1)|Acc2]).

% neuronself_recurrent
populate_PHyperlayers_nsr(PrevHyperlayer, [{Coord, OldO, OldWeights}|CurHyperlayer], Substrate, CPP_PIds, CEP_PIds, Plasticity, Acc1, Acc2) ->
    NewWeights = case Plasticity of
        none ->
            % 将自己{Coord, OldO, OldWeights}加入I_Neurodes中，实现神经节点自循环
            get_weights([{Coord, OldO, OldWeights}|PrevHyperlayer], Coord, CPP_PIds, CEP_PIds, []);
        _ -> % iterative, abcn
            get_weights([{Coord, OldO, OldWeights}|PrevHyperlayer], Coord, CPP_PIds, CEP_PIds, [], OldWeights, OldO)
    end,
    populate_PHyperlayers_nsr(PrevHyperlayer, CurHyperlayer, Substrate, CPP_PIds, CEP_PIds, Plasticity, [{Coord, OldO, NewWeights}|Acc1], Acc2);
% 处理完一层时
populate_PHyperlayers_nsr(_PrevHyperlayer, [], [CurHyperlayer|Substrate], CPP_PIds, CEP_PIds, Plasticity, Acc1, Acc2) ->
    PrevHyperlayer = lists:reverse(Acc1), % 处理完的当前层成为上一层，而下一层成为当前层
    populate_PHyperlayers_nsr(PrevHyperlayer, CurHyperlayer, Substrate, CPP_PIds, CEP_PIds, Plasticity, [], [PrevHyperlayer|Acc2]);
% 处理完所有层时，返回种植好权重的Substrate
populate_PHyperlayers_nsr(_PrevHyperlayer, [], [], CPP_PIds, CEP_PIds, Plasticity, Acc1, Acc2) ->
    lists:reverse([lists:reverse(Acc1)|Acc2]).

% 无可塑性
get_weights([{I_Coord, I, _I_Weights}|I_Neurodes], Coord, CPP_PIds, CEP_PIds, Acc) ->
    static_fanout(CPP_PIds, I_Coord, Coord),
    U_W = fanin(CEP_PIds, void),
    get_weights(I_Neurodes, Coord, CPP_PIds, CEP_PIds, [U_W|Acc]);
get_weights([], _Coord, _CPP_PIds, _CEP_PIds, Acc) ->
    lists:reverse(Acc).

static_fanout([CPP_PId|CPP_PIds], I_Coord, Coord) ->
    CPP_PId ! {self(), I_Coord, Coord},
    static_fanout(CPP_PIds, I_Coord, Coord);
static_fanout([], _I_Coord, _Coord) ->
    done.

fanin([CEP_PId|CEP_PIds], W) ->
    receive
        % {CEP_PId, set_weight, [Weight]}
        % {CEP_PId, set_abcn, Output}
        % {CEP_PId, set_iterative, [DeltaWeight]}
        {CEP_PId, Command, Signal} ->
            U_W = substrate:Command(Signal, W)
    end,
    fanin(CEP_PIds, U_W);
fanin([], W) ->
    W.

% 有可塑性
get_weights([{I_Coord, I, _I_Weights}|I_Neurodes], Coord, CPP_PIds, CEP_PIds, Acc, [W|Weights], O) ->
    plasticity_fanout(CPP_PIds, I_Coord, Coord, [I, O, W]),
    U_W = fanin(CEP_PIds, W),
    get_weights(I_Neurodes, Coord, CPP_PIds, CEP_PIds, [U_W|Acc], Weights, O);
get_weights([], _Coord, _CPP_PIds, _CEP_PIds, Acc, [], _O) ->
    lists:reverse(Acc).

plasticity_fanout([CPP_PId|CPP_PIds], I_Coord, Coord, IOW) ->
    CPP_PId ! {self(), I_Coord, Coord, IOW},
    plasticity_fanout(CPP_PIds, I_Coord, Coord, IOW);
plasticity_fanout([], _I_Coord, _Coord, _IOW) ->
    done.

set_weight(Signal, _WP) ->
    [U_W] = Signal,
    functions:saturation(U_W, ?SAT_LIMIT).

set_abcn(Signal, _WP) ->
    [U_W, A, B, C, N] = Signal,
    % WP：{W, LF, Parameters}
    {functions:saturation(U_W, ?SAT_LIMIT), abcn, [A, B, C, N]}.

set_iterative(Signal, W) ->
    [Delta_Weight] = Signal,
    functions:saturation(W + Delta_Weight, ?SAT_LIMIT).

weight_expression(Signal, _WP) ->
    [U_W, Expression] = Signal,
    case Expression > 0 of
        true ->
            functions:saturation(U_W, ?SAT_LIMIT);
        false ->
            0
    end.

%%--------------------------------------------------------------------
%% 计算基底输出
%%--------------------------------------------------------------------

% 计算基底的输出
calculate_substrate_output(IHyperlayer, PHyperlayers, LinkForm, Plasticity, CPP_PIds, CEP_PIds) ->
    case LinkForm of
        l2l_feedforward ->
            calculate_output_std(IHyperlayer, PHyperlayers, Plasticity, CPP_PIds, CEP_PIds, []);
        fully_interconnected ->
            calculate_output_fi(IHyperlayer, PHyperlayers, Plasticity, CPP_PIds, CEP_PIds, []);
        jordan_recurrent ->
            [OHyperlayer|_] = lists:reverse(PHyperlayers),
            calculate_output_std(lists:flatten([IHyperlayer, OHyperlayer]), PHyperlayers, Plasticity, CPP_PIds, CEP_PIds, []);
        neuronself_recurrent ->
            calculate_output_nsr(IHyperlayer, PHyperlayers, Plasticity, CPP_PIds, CEP_PIds, [])
    end.

% 具体计算基底的输出（标准版）
% 返回：{输出层信号列表, 更新后的Substrate}
% Substrate：Hyperlayer列表，Hyperlayer：Neurode列表，Neurode：{坐标, 上一次的输出信号, 对上一层输出的权重列表}
calculate_output_std(I_Neurodes, [Cur_Hyperlayer|Substrate], Plasticity, CPP_PIds, CEP_PIds, Acc) ->
    % 计算当前层的输出
    U_CurHyperlayer = [calculate_output(I_Neurodes, Neurode, Plasticity, CPP_PIds, CEP_PIds) || Neurode <- Cur_Hyperlayer],
    % 更新的当前层成为下一轮迭代的上一层
    calculate_output_std(U_CurHyperlayer, Substrate, Plasticity, CPP_PIds, CEP_PIds, [U_CurHyperlayer|Acc]);
% 当所有层都处理完时
calculate_output_std(Output_Hyperlayer, [], _Plasticity, CPP_PIds, CEP_PIds, Acc) ->
    {[Output || {_Coord, Output, _Weights} <- Output_Hyperlayer], lists:reverse(Acc)}.

% 计算单个神经节点的输出
% 返回：{Coord, Output, Weights}
calculate_output(I_Neurodes, Neurode, Plasticity, CPP_PIds, CEP_PIds) ->
    {Coord, _Prev_O, Weights} = Neurode,
    case Plasticity of
        none ->
            Output = calculate_neurode_output_std(I_Neurodes, Neurode, 0),
            {Coord, Output, Weights};
        iterative ->
            Output = calculate_neurode_output_std(I_Neurodes, Neurode, 0),
            % 通过get_weights（即通过NN，把NN当做一个学习函数，NN是通用函数拟合器）更新该神经节点的各个权重
            % 在每个sense-think-act周期环节，通过NN而非直接的学习函数来计算更新权重，显著需要更多的算力，但更加强大灵活（能进化出任何的学习规则）
            % 优化选项：将进化好的NN转换成单个函数嵌入到每个神经节点中（甚至每个权重有不一样的学习函数都能支持），就如同简单的可塑函数的调用
            U_Weights = get_weights(I_Neurodes, Coord, CPP_PIds, CEP_PIds, [], Weights, Output),
            {Coord, Output, U_Weights};
        abcn ->
            Output = calculate_neurode_output_plast(I_Neurodes, Neurode, 0),
            % 根据学习函数更新该神经节点的各个权重，返回：{Coord, Output, U_WeightPs}
            update_neurode(I_Neurodes, {Coord, Output, Weights}, [])
    end.

% 具体计算单个神经节点的输出（标准版）
% Weights顺序和上一层I_Neurodes顺序一致
calculate_neurode_output_std([{_I_Coord, O, _I_Weights}|I_Neurodes], {Coord, Prev_O, [Weight|Weights]}, Acc) ->
    calculate_neurode_output_std(I_Neurodes, {Coord, Prev_O, Weights}, O * Weight + Acc);
calculate_neurode_output_std([], {Coord, Prev_O, []}, Acc) ->
    functions:tanh(Acc). % 固定是tanh

% 具体计算单个神经节点的输出（可塑性版）
% plast：plasticity
% LF：Learning Function，如abcn
calculate_neurode_output_plast([{_I_Coord, O, _I_Weights}|I_Neurodes], {Coord, Prev_O, [{W, _LF, _Parameters}|WPs]}, Acc) ->
    calculate_neurode_output_plast(I_Neurodes, {Coord, Prev_O, WPs}, O * W + Acc);
calculate_neurode_output_plast([], {Coord, Prev_O, []}, Acc) ->
    functions:tanh(Acc). % 固定是tanh

% 根据学习函数更新单个神经节点的各个权重
% 返回：{Coord, Output, WeightPs}
update_neurode([{_I_Coord, I_O, _I_Weights}|I_Neurodes], {Coord, O, [{W, LF, Parameters}|WPs]}, Acc) ->
    U_W = substrate:LF(I_O, O, W, Parameters),
    update_neurode(I_Neurodes, {Coord, O, WPs}, [{U_W, LF, Parameters}|Acc]);
update_neurode([], {Coord, O, []}, Acc) ->
    {Coord, O, lists:reverse(Acc)}.

% LF：Learning Function
% 参数：I，O，W，Parameters
abcn(Input, Output, W, [A, B, C, N]) ->
    Delta_Weight = N * (A * Input * Output + B * Input + C * Output), % 与一般Hebb规则: DW = H(AIO + BI + CO + D)相比，没有常数项
    W + Delta_Weight.

% 具体计算基底的输出（全连接版）
% 返回：{输出层信号列表, 更新后的Substrate}
% Substrate：Hyperlayer列表，Hyperlayer：Neurode列表，Neurode：{坐标, 上一次的输出信号, 对上一层输出的权重列表}
calculate_output_fi(I_Neurodes, [Cur_Hyperlayer|Substrate], Plasticity, CPP_PIds, CEP_PIds, Acc) ->
    AllNeurodes = lists:flatten([I_Neurodes, Cur_Hyperlayer|Substrate]),
    % 计算当前层中神经节点的输出，虽然是全连接，但计算时仍按照层层递进的顺序来计算每一个神经节点的输出
    U_CurHyperlayer = [calculate_output(AllNeurodes, Neurode, Plasticity, CPP_PIds, CEP_PIds) || Neurode <- Cur_Hyperlayer],
    % 更新的当前层与I_Neurodes合并，成为下一轮迭代的I_Neurodes
    calculate_output_fi(lists:flatten([I_Neurodes, U_CurHyperlayer]), Substrate, Plasticity, CPP_PIds, CEP_PIds, [U_CurHyperlayer|Acc]);
% 当所有层都处理完时
calculate_output_fi(Output_Hyperlayer, [], _Plasticity, CPP_PIds, CEP_PIds, Acc) ->
    {[Output || {_Coord, Output, _Weights} <- Output_Hyperlayer], lists:reverse(Acc)}.

% 具体计算基底的输出（神经元自循环版）
% 返回：{输出层信号列表, 更新后的Substrate}
% Substrate：Hyperlayer列表，Hyperlayer：Neurode列表，Neurode：{坐标, 上一次的输出信号, 对上一层输出的权重列表}
calculate_output_nsr(I_Neurodes, [Cur_Hyperlayer|Substrate], Plasticity, CPP_PIds, CEP_PIds, Acc) ->
    % 计算当前层的输出，注意对于每个神经节点，将它自己放入计算自己输出的I_Neurodes中
    U_CurHyperlayer = [calculate_output([Neurode|I_Neurodes], Neurode, Plasticity, CPP_PIds, CEP_PIds) || Neurode <- Cur_Hyperlayer],
    % 更新的当前层成为下一轮迭代的上一层
    calculate_output_nsr(U_CurHyperlayer, Substrate, Plasticity, CPP_PIds, CEP_PIds, [U_CurHyperlayer|Acc]);
% 当所有层都处理完时
calculate_output_nsr(Output_Hyperlayer, [], _Plasticity, CPP_PIds, CEP_PIds, Acc) ->
    {[Output || {_Coord, Output, _Weights} <- Output_Hyperlayer], lists:reverse(Acc)}.

