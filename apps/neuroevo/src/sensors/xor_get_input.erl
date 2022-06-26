-module(xor_get_input).
-export([sense/4]).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

% The function contacts the XOR simulator and requests the sensory vector, which in this case should be a binary vector of length 2.
% The sensor checks that the incoming sensory signal, the percept, is indeed of length 2. If the vector length differs, then this is
% printed to the console and a dummy vector of appropriate length is constructed.
sense(Exoself_PId, VL, _Parameters, Scape) ->
    Scape ! {self(), sense},
    receive
        {Scape, percept, SensoryVector} ->
            case length(SensoryVector) =:= VL of
                true ->
                    SensoryVector;
                false ->
                    ?ERR("Error in sensor: xor_get_input, VL:~p SensoryVector:~p~n", [VL, SensoryVector]),
                    lists:duplicate(VL, 0)
            end
    end.

