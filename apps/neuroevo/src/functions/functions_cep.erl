-module(functions_cep).
-compile(export_all).

% CEPName：set_weight，delta_weight，set_abcn

set_weight(Output, _Parameters, Substrate_PId) ->
    [Val] = Output,
    Threshold = 0.33,
    Weight = if
        Val > Threshold -> % (0, 1]
            (functions:scale(Val, 1, Threshold) + 1) / 2;
        Val < -Threshold -> % [-1, 0)
            (functions:scale(Val, -Threshold, -1) - 1) / 2;
        true ->
            0 % 不连接
    end,
    Substrate_PId ! {self(), set_weight, [Weight]}.

delta_weight(Output, _Parameters, Substrate_PId) ->
    [Val] = Output,
    Threshold = 0.33,
    DeltaWeight = if
        Val > Threshold -> % (0, 1]
            (functions:scale(Val, 1, Threshold) + 1) / 2;
        Val < -Threshold -> % [-1, 0)
            (functions:scale(Val, -Threshold, -1) - 1) / 2;
        true ->
            0
    end,
    Substrate_PId ! {self(), set_iterative, [DeltaWeight]}.

set_abcn(Output, _Parameters, Substrate_PId) ->
    % Output：[W, A, B, C, N]
    Substrate_PId ! {self(), set_abcn, Output}.

