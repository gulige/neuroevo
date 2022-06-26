-module(flatland_two_wheels).
-export([act/5]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

act(ExoSelf_PId, Output, VL, Parameters, Scape) ->
    OVL = length(Output),
    {Fitness, HaltFlag} =
        case OVL =:= VL of
            true ->
                gen_server:call(Scape, {actuator, ExoSelf_PId, two_wheels, Output});
            false -> % VL > OVL
                PaddingOutput = lists:append(Output, lists:duplicate(VL - OVL, 0)),
                gen_server:call(Scape, {actuator, ExoSelf_PId, two_wheels, PaddingOutput})
        end,
    {Fitness, HaltFlag}.

