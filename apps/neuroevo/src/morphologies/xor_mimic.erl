-module(xor_mimic).
-export([sensors/0, actuators/0]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

% Every sensor and actuator uses some kind of function associated with it. A function that either polls the environment for sensory signals
% (in the case of a sensor) or acts upon the environment (in the case of an actuator). It is a function that we need to define and program
% before it is used, and the name of the function is the same as the name of the sensor or actuator it self.

sensors() ->
    [
        #sensor{name = xor_get_input, type = standard, scape = {private, xor_sim}, vl = 2}
    ].

actuators() ->
    [
        #actuator{name = xor_send_output, type = standard, scape = {private, xor_sim}, vl = 1}
    ].

