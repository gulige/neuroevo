-module(morphology).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

% Get Init Standard Sensors（初始只有1个）
get_InitSensors(Morphology) ->
    [Sensor|_] = Morphology:sensors(),
    [Sensor].

% % Get Init Standard Actuators（初始只有1个）
get_InitActuators(Morphology) ->
    [Actuator|_] = Morphology:actuators(),
    [Actuator].

% Get Standard Sensors（全部）
get_Sensors(Morphology) ->
    Morphology:sensors().

% Get Standard Actuators（全部）
get_Actuators(Morphology) ->
    Morphology:actuators().

% Get Init Substrate_CPPs
get_InitSubstrateCPPs(Dimensions, Plasticity) ->
    [Substrate_CPP|_] = get_SubstrateCPPs(Dimensions, Plasticity),
    [Substrate_CPP].

% % Get Init Substrate_CEPs
get_InitSubstrateCEPs(Dimensions, Plasticity) ->
    [Substrate_CEP|_] = get_SubstrateCEPs(Dimensions, Plasticity),
    [Substrate_CEP].

% Get Substrate_CPPs
get_SubstrateCPPs(Dimensions, Plasticity) ->
    ?DBG("get_SubstrateCPPs: Dimensions=~p, Plasticity=~p~n", [Dimensions, Plasticity]),
    if
        (Plasticity =:= iterative) orelse (Plasticity =:= abcn) ->
            Std = [ % 标准的，和具体的维数无关
                #sensor{type = substrate, name = cartesian, vl = Dimensions * 2 + 3}, % 2：2个点的坐标，3：[I,O,W]
                #sensor{type = substrate, name = centripetal_distances, vl = 2 + 3},
                #sensor{type = substrate, name = cartesian_distance, vl = 1 + 3},
                #sensor{type = substrate, name = cartesian_CoordDiffs, vl = Dimensions + 3},
                #sensor{type = substrate, name = cartesian_GaussedCoordDiffs, vl = Dimensions + 3},
                #sensor{type = substrate, name = iow, vl = 3}
            ],
            Adt = case Dimensions of % 附加的，和具体的维数有关：2维则附加极坐标，3维则附加球坐标
                2 -> [#sensor{type = substrate, name = polar, vl = Dimensions * 2 + 3}];
                3 -> [#sensor{type = substrate, name = spherical, vl = Dimensions * 2 + 3}];
                _ -> []
            end,
            lists:append(Std, Adt);
        (Plasticity =:= none) orelse (Plasticity =:= modular_none) ->
            Std = [ % 标准的，和具体的维数无关
                #sensor{type = substrate, name = cartesian, vl = Dimensions * 2},
                #sensor{type = substrate, name = centripetal_distances, vl = 2},
                #sensor{type = substrate, name = cartesian_distance, vl = 1},
                #sensor{type = substrate, name = cartesian_CoordDiffs, vl = Dimensions},
                #sensor{type = substrate, name = cartesian_GaussedCoordDiffs, vl = Dimensions}
            ],
            Adt = case Dimensions of % 附加的，和具体的维数有关：2维则附加极坐标，3维则附加球坐标
                2 -> [#sensor{type = substrate, name = polar, vl = Dimensions * 2}];
                3 -> [#sensor{type = substrate, name = spherical, vl = Dimensions * 2}];
                _ -> []
            end,
            lists:append(Std, Adt)
    end.

% Get Substrate_CEPs
get_SubstrateCEPs(Dimensions, Plasticity) ->
    case Plasticity of
        iterative ->    [#actuator{type = substrate, name = delta_weight, vl = 1}];
        abcn ->         [#actuator{type = substrate, name = set_abcn, vl = 5}]; % Output：[W, A, B, C, N]
        none ->         [#actuator{type = substrate, name = set_weight, vl = 1}];
        modular_none -> [#actuator{type = substrate, name = weight_expression, vl = 2}]
    end.

