-module(flatland_predator).
-export([sensors/0, actuators/0]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

sensors() ->
    flatland_prey:sensors().

actuators() ->
    flatland_prey:actuators().

