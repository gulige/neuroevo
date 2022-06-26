-module(xor_send_output).
-export([act/5]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

% The function simply forwards the Output vector to the XOR simulator, and waits for the resulting Fitness and EndFlag from the simulation process.
act(ExoSelf_PId, Output, VL, _Parameters, Scape) ->
    Scape ! {self(), action, Output},
    receive
        {Scape, Fitness, HaltFlag} ->
            {Fitness, HaltFlag}
    end.

