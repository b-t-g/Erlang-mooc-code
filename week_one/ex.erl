-module(ex).
-export([bits/1, perimeter/1, area/1, enclose/1, bits_iterative/1]).

-export([test_enclose_triangle1/0, test_enclose_triangle2/0, test_enclose_circle1/0]).
-export([test_enclose_circle2/0, test_perimeter_rect1/0, test_perimeter_rect2/0]).
-export([test_perimeter_rect3/0, test_perimeter_circle1/0, test_perimeter_circle2/0]).
-export([test_perimeter_circle3/0, test_perimeter_circle4/0, test_area1/0, test_area2/0]).
-export([test_area3/0, test_bits1/0, test_bits2/0, test_bits3/0, test_bits4/0, test_bits5/0]).

%% Given most numbers of interest do not have enough bits to cause a stack overflow

%% and we don't have tree recursion (as in the Fibonacci case), there isn't redundant
%% work done by direct recursion. Since this implementation doesn't require a helper
%% function (like the iterative version), an argument could be made for this version
%% being better.
bits(X) when X == 0; X == 1 ->
    X;
%% If we got here, then X must not be 0 or 1.
bits(X) ->
    1 + bits(X bxor (X band (bnot (X - 1)))).

bits_iterative(X) ->
    bits_iterative(X, 0).

bits_iterative(X, Y) when X == 0; X == 1 ->
    Y;
bits_iterative(X, Y) ->
    %% X ^ (X & ~(X - 1)) removes the least significant bit.
    Lsb_cleared = X bxor (X band (bnot (X - 1))),
    bits_iterative(Lsb_cleared, Y + 1).

%% For representing a triangle, I chose the representation to just be the three
%% points defining the vertices; it both seemed like the most natural
%% representation as well as making it the easiest to find the smallest
%% enclosing rectangle while also not making it terribly more difficult
%% to find the area.

%% Heron's formula (described earlier in the course)
area({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    {{A, _, _}, {B, _, _}, {C, _, _}} =
	segment_lengths({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}),
    S = (A + B + C)/2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

%% This is an extra function necessary for representing a triangle in this way
%% as opposed to just a center and the length of the sides.
segment_lengths({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    A = {segment_length({XP1, YP1}, {XP2, YP2}), {XP1, YP1}, {XP2, YP2}},
    B = {segment_length({XP2, YP2}, {XP3, YP3}), {XP2, YP2}, {XP3, YP3}},
    C = {segment_length({XP3, YP3}, {XP1, YP1}), {XP3, YP3}, {XP1, YP1}},
    {A, B, C}.
segment_length({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

perimeter({rectangle, _, H, W}) ->
    2*(H + W);
perimeter({circle, _, R}) ->
    2*math:pi()*R;
perimeter({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    {{A, _, _}, {B, _, _}, {C, _, _}} =
	segment_lengths({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}),
    A + B + C.

%% Added just for the sake of completeness.
enclose({rectangle, X, Y, Z}) ->
    {rectangle, X, Y, Z};
%% The smallest rectangle enclosing a circle is just a square, centered
%% at the same location with widths and heights twice as big as the circle's radius.
enclose({circle, {X, Y}, R}) ->
    {rectangle, {X, Y}, 2*R, 2*R};

%% This is where things get fun.
%% The base of the enclosing rectangle (call it AB) starts with the longest segment of the
%% triangle, the parallel side (call it CD) will go through the last point of the triangle E
%% that was not in the base. We then find one of the vertices by taking the
%% line perpendicular to the base and find the intersection with CD.
enclose({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    {A, B, C} = segment_lengths({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}),
    {L, {X1, Y1}, {X2, Y2}} = extremal_length([A, B, C]),
    {Slope_base, Slope_perp} = calculate_slopes({X1, Y1}, {X2, Y2}),
    [{Last_x, Last_y}] = [{XP1, YP1}, {XP2, YP2}, {XP3, YP3}] -- [{X1, Y1}, {X2, Y2}],
    {XV, YV} = opposite_vertex({Last_x, Last_y}, {X1, Y1}, Slope_base, Slope_perp),
    {rectangle, {(XV + X2)/2, (YV + Y2)/2}, L, segment_length({X1, Y1}, {XV, YV})}.

%% Find the vertex opposite to another given vertex A on a rectangle given a point B
%% that is contained on a line parallel to the base, the coordinates of the vertex A,
%% the slope of the line containing A, and the slope of the line containing B.
%% 
%% The case where one of the lines is of the form x = c (for some constant c)
%% is an exceptional case, thus we are pattern matching against these specifically.
opposite_vertex({Last_x, _Last_y}, {_X1, Y1}, inf, _) ->
    {Last_x, Y1};
opposite_vertex({_Last_x, Last_y}, {X1, _Y1}, 0, _) ->
    {X1, Last_y};
opposite_vertex({Last_x, Last_y}, {X1, Y1}, Slope_base, Slope_perp) ->
    Parallel_y_intercept = Last_y - Slope_base*Last_x,
    Perp_y_intercept = Y1 - Slope_perp*X1,
    %% Solve the equation mx + b = m'x + b' for x where m = Slope_base, 
    %% m' = Slope_perp, b = Parallel_y_intercept, and b' = Perp_y_intercept.
    %% i.e., find the x value for which the line parallel to the base and the
    %% line perpendicular to the base (going through A = {X1, Y1}) intersect.
    X_value = (Perp_y_intercept - Parallel_y_intercept)/(Slope_base - Slope_perp),
    {X_value, Slope_base*X_value + Parallel_y_intercept}.

%% This shows the benefit of representing a triangle by its points;
%% we are able to calculate the slope of the lines which would
%% have been much more difficult with any other representation.
calculate_slopes({X1, _Y1}, {X2, _Y2}) when X1 == X2 ->
    {inf, 0};
calculate_slopes({_X1, Y1}, {_X2, Y2}) when Y1 == Y2 ->
    {0, inf};
calculate_slopes({X1, Y1}, {X2, Y2}) ->
    Slope_base = (Y2 - Y1)/(X2 - X1),
    Slope_perp = -1/(Slope_base),
    {Slope_base, Slope_perp}.

extremal_length(Lengths) ->
    lists:foldl(fun augment_max/2, {0, {0,0}, {0,0}}, Lengths).

augment_max({L1, {X1, Y1}, {X2, Y2}}, {L2, {_X3, _Y3}, {_X4, _Y4}}) when L1 > L2 ->
	{L1, {X1, Y1}, {X2, Y2}};
augment_max({L1, {_X1, _Y1}, {_X2, _Y2}}, {L2, {X3, Y3}, {X4, Y4}}) when L2 >= L1 ->
	{L2, {X3, Y3}, {X4, Y4}}.

%% Test two relatively straightforward cases for enclose for the triangle case.
test_enclose_triangle1() ->
    enclose({triangle, {0,0}, {1,0}, {math:sqrt(2)/2, math:sqrt(2)/2}}) ==
	{rectangle,{0.5,0.3535533905932738},1.0,0.7071067811865476}.
test_enclose_triangle2() ->
    enclose({triangle, {0,0}, {3,0}, {0,4}}) == 
	{rectangle,{0.54,1.28},5.0,2.4}.
%% Test correctness.
test_enclose_circle1() ->
    enclose({circle, {0,0}, 1}) == 
	{rectangle,{0,0},2,2}.
test_enclose_circle2() ->
    enclose({circle, {3,4}, 3}) == 
	{rectangle,{3,4},6,6}.
test_perimeter_rect1() ->
    perimeter({rectangle, {3,4}, 3,4}) == 14.
%% Ensure perimeter is invariant under translation
test_perimeter_rect2() ->
    perimeter({rectangle, {0,0}, 3,4}) == 14.
test_perimeter_rect3() ->
    perimeter({rectangle, {0,0}, 5, 6}) == 22.
%% Easily verifiable case.
test_perimeter_circle1() ->
    perimeter({circle, {0,0}, 1/(2*math:pi())}) == 1.0.
test_perimeter_circle2() ->
    perimeter({circle, {0,0}, 1}) == 6.283185307179586.
%% Ensure perimeter is invariant under translation
test_perimeter_circle3() ->
    perimeter({circle, {0,1}, 1}) == 6.283185307179586.
%% Less straightforward case.
test_perimeter_circle4() ->
    perimeter({circle, {0,1}, math:sqrt(2)}) == 8.885765876316732.
%% Straightforward case.
test_area1() ->
    area({triangle, {0,0}, {0,3}, {4,0}}) == 6.
%% Easy to verify, but more computationally complex.
test_area2() ->
    area({triangle, {0,0}, {0,1}, {math:sqrt(2)/2, math:sqrt(2)/2}}) == 0.3535533905932738.
%% A seemingly random case that turned out to be nice.
test_area3() ->
    area({triangle, {0,0}, {0,1}, {2, math:sqrt(2)/2}}) == 1.0000000000000004.
test_bits1() ->
    bits(7) == 3.
test_bits2() ->
    bits(2) == 1.
test_bits3() ->
    bits(3) == 2.
test_bits4() ->
    bits(16) == 1.
test_bits5() ->
    bits(99) == 4.
