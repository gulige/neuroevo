-module(functions_cpp).
-compile(export_all).

% HYPERCUBE:
% Specifications:[I,H,O]
% I,O:[LL1...LLn] Layer length, where LL is on the X axis, and 1-n is on the Y axis, making this a 2d specification.
% [[LL1...LLx],[LL1...LLx]] Depth 2 list, 3d I_Hypercube.
% [{Dimension,[{Coord1...CoordN}]},{Dimension,[{Coord1...CoordN}]}]
% H:[N..Z,Y,X] Where N is the depth, and Z,Y,X specifies the dimensions of a symmetric hypercube. Z by Y by X.
% abc, none

cartesian(I_Coord, Coord) ->
    lists:append(I_Coord, Coord).

polar(I_Coord, Coord) ->
    lists:append(cart2pol(I_Coord), cart2pol(Coord)).

spherical(I_Coord, Coord) ->
    lists:append(cart2spher(I_Coord), cart2spher(Coord)).

centripetal_distances(I_Coord, Coord) ->
    [centripetal_distance(I_Coord, 0), centripetal_distance(Coord, 0)].

cartesian_distance(I_Coord, Coord) ->
    [calculate_distance(I_Coord, Coord, 0)].

% I:[X1,Y1,Z1] [X2,Y2,Z2] O:[X2-X1,Y2-Y1,Z2-Z1]
cartesian_CoordDiffs(FromCoords, ToCoords) ->
    cartesian_CoordDiffs_(FromCoords, ToCoords, []).

cartesian_CoordDiffs_([FromCoord|FromCoords], [ToCoord|ToCoords], Acc) ->
    cartesian_CoordDiffs_(FromCoords, ToCoords, [ToCoord - FromCoord|Acc]);
cartesian_CoordDiffs_([], [], Acc) ->
    lists:reverse(Acc).

% I:[X1,Y1,Z1] [X2,Y2,Z2] O:[gauss(X2-X1),gauss(Y2-Y1),gauss(Z2-Z1)]
cartesian_GaussedCoordDiffs(FromCoords, ToCoords) ->
    cartesian_GaussedCoordDiffs_(FromCoords, ToCoords, []).

cartesian_GaussedCoordDiffs_([FromCoord|FromCoords], [ToCoord|ToCoords], Acc) ->
    cartesian_GaussedCoordDiffs_(FromCoords, ToCoords, [activation_functions:gaussian(ToCoord - FromCoord)|Acc]);
cartesian_GaussedCoordDiffs_([], [], Acc) ->
    lists:reverse(Acc).

% Iterative
%

cartesian(I_Coord, Coord, [I, O, W]) ->
    [I, O, W|lists:append(I_Coord, Coord)].

polar(I_Coord, Coord, [I, O, W]) ->
    [I, O, W|lists:append(cart2pol(I_Coord), cart2pol(Coord))].

spherical(I_Coord, Coord, [I, O, W]) ->
    [I, O, W|lists:append(cart2spher(I_Coord), cart2spher(Coord))].

centripetal_distances(I_Coord, Coord, [I, O, W]) ->
    [I, O, W, centripetal_distance(I_Coord, 0), centripetal_distance(Coord, 0)].

cartesian_distance(I_Coord, Coord, [I, O, W]) ->
    [I, O, W, calculate_distance(I_Coord, Coord, 0)].

cartesian_CoordDiffs(FromCoords, ToCoords, [I, O, W]) ->
    [I, O, W|cartesian_CoordDiffs(FromCoords, ToCoords)].

cartesian_GaussedCoordDiffs(FromCoords, ToCoords, [I, O, W]) ->
    [I, O, W|cartesian_GaussedCoordDiffs(FromCoords, ToCoords)].

iow(_I_Coord, _Coord, IOW) ->
    IOW.

cart2pol([Y, X]) ->
    {R, Theta} = functions:cart2pol({X, Y}),
    [R, Theta].

cart2spher([Z, Y, X]) ->
    {P, Theta, Phi} = functions:cart2spher({X, Y, Z}),
    [P, Theta, Phi].

% 计算坐标点的中心距离（与O点的距离）
centripetal_distance([Val|Coord], Acc) ->
    centripetal_distance(Coord, Val * Val + Acc);
centripetal_distance([], Acc) ->
    math:sqrt(Acc).

% 计算两坐标点的笛卡尔距离
calculate_distance([Val1|Coord1], [Val2|Coord2], Acc) ->
    Distance = Val2 - Val1,
    calculate_distance(Coord1, Coord2, Distance * Distance + Acc);
calculate_distance([], [], Acc) ->
    math:sqrt(Acc).

