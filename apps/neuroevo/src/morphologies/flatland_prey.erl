-module(flatland_prey).
-export([sensors/0, actuators/0]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

% Every sensor and actuator uses some kind of function associated with it. A function that either polls the environment for sensory signals
% (in the case of a sensor) or acts upon the environment (in the case of an actuator). It is a function that we need to define and program
% before it is used, and the name of the function is the same as the name of the sensor or actuator it self.

sensors() ->
    Pi = math:pi(),
    Distance_Scanners =
        [#sensor{name = flatland_distance_scanner,
                 type = standard,
                 scape = {public, flatland},
                 format = no_geo,
                 vl = Density,
                 parameters = [Spread, Density, RadialOffset]} ||
         Spread <- [Pi/2], Density <- [5], RadialOffset <- [Pi*0/2]],
    Color_Scanners =
        [#sensor{name = flatland_color_scanner,
                 type = standard,
                 scape = {public, flatland},
                 format = no_geo,
                 vl = Density,
                 parameters = [Spread, Density, RadialOffset]} ||
         Spread <- [Pi/2], Density <- [5], RadialOffset <- [Pi*0/2]],
    Energy_Scanners =
        [#sensor{name = flatland_energy_scanner,
                 type = standard,
                 scape = {public, flatland},
                 format = no_geo,
                 vl = Density,
                 parameters = [Spread, Density, RadialOffset]} ||
         Spread <- [Pi/2], Density <- [5], RadialOffset <- [Pi*0/2]],
    Distance_Scanners ++ Color_Scanners ++ Energy_Scanners.

actuators() ->
    Movement =
        [#actuator{name = flatland_two_wheels,
                   type = standard,
                   scape = {public, flatland},
                   format = no_geo,
                   vl = 2,
                   parameters = []}],
    Movement.

