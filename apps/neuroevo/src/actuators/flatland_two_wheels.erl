-module(flatland_two_wheels).
-export([act/5]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

act(ExoSelf_PId, Output, VL, Parameters, Scape) ->
    OVL = length(Output),
    {Fitness, HaltFlag} =
        case OVL =:= VL of
            true ->
                gen_server:call(Scape, {act, ExoSelf_PId, two_wheels, Output}, 60000);
            false -> % VL > OVL
                PaddingOutput = lists:append(Output, lists:duplicate(VL - OVL, 0)),
                gen_server:call(Scape, {act, ExoSelf_PId, two_wheels, PaddingOutput}, 60000)
        end,
    {Fitness, HaltFlag}.

