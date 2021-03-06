%% 用于输入信号与权重的聚合方式

-module(signal_aggregator).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

% 计算输入与权重的点积
dot_product(IAcc, IPIdPs) ->
    dot_product(IAcc, IPIdPs, 0).

dot_product([{IPId, Input}|IAcc], [{IPId, WeightsP}|IPIdPs], Acc) ->
    Dot = dot(Input, WeightsP, 0),
    dot_product(IAcc, IPIdPs, Dot + Acc);
dot_product([], [{bias, [{Bias, _LPs}]}], Acc) ->
    Acc + Bias;
dot_product([], [], Acc) ->
    Acc.

% The dot/3 function accepts an input vector and a weight list, and computes the dot product of the two vectors.
dot([I|Input], [{W, _LPs}|WeightsP], Acc) ->
    dot(Input, WeightsP, I * W + Acc);
dot([], [], Acc) ->
    Acc.

% 用本次输入与上次输入的差，作为输入，计算输入与权重的点积
diff_product(IAcc, IPIdPs) ->
    case get(diff_product) of
        undefined ->
            put(diff_product, IAcc),
            dot_product(IAcc, IPIdPs, 0);
        Prev_IAcc ->
            put(diff_product, IAcc),
            Diff_IAcc = input_diff(IAcc, Prev_IAcc, []),
            dot_product(Diff_IAcc, IPIdPs, 0)
    end.

input_diff([{IPId, Input}|IAcc], [{IPId, Prev_Input}|Prev_IAcc], Acc) ->
    Vector_Diff = diff(Input, Prev_Input, []),
    input_diff(IAcc, Prev_IAcc, [{IPId, Vector_Diff}|Acc]);
input_diff([], [], Acc) ->
    lists:reverse(Acc).

diff([A|Input], [B|Prev_Input], Acc) ->
    diff(Input, Prev_Input, [A-B|Acc]);
diff([], [], Acc) ->
    lists:reverse(Acc).

% 计算输入与权重的复积（输入分量与权重分量的乘积不求和，而是再求乘积）
mult_product(IAcc, IPIdPs) ->
    mult_product(IAcc, IPIdPs, 1).

mult_product([{IPId, Input}|IAcc], [{IPId, WeightsP}|IPIdPs], Acc) ->
    Dot = mult(Input, WeightsP, 1),
    mult_product(IAcc, IPIdPs, Dot * Acc);
mult_product([], [{bias, [{Bias, _LPs}]}], Acc) ->
    Acc * Bias;
mult_product([], [], Acc) ->
    Acc.

mult([I|Input], [{W, _LPs}|Weights], Acc) ->
    mult(Input, Weights, I * W * Acc);
mult([], [], Acc) ->
    Acc.

