-module(ex).
-export([bits/1, perimeter/1, area/1, enclose/1]).

bits(X) ->
    bits_help(X, 0).

bits_help(0,Y) ->
    Y;
bits_help(1, Y) ->
    Y + 1;
bits_help(X, Y) ->
    %% X ^ (X & ~(X - 1)) removes the least significant bit.
    Lsb_cleared = X bxor (X band (bnot (X - 1))),
    bits_help(Lsb_cleared, Y + 1).

%% For representing a triangle, I chose the representation to just be the three
%% points defining the vertices; it both seemed like the most natural
%% representation as well as making it the easiest to find the smallest
%% enclosing rectangle while also not making it terribly more difficult
%% to find the area.

%% Heron's formula (described earlier in the course)
area({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    {A,B,C} = segment_lengths({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}),
    S = (A + B + C)/2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

segment_lengths({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    A = segment_length({XP1, YP1}, {XP2, YP2}),	
    B = segment_length({XP2, YP2}, {XP3, YP3}),
    C = segment_length({XP3, YP3}, {XP1, YP1}),
    {A,B,C}.

segment_length({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

perimeter({rectangle, _, H, W}) ->
    2*(H + W);
perimeter({circle, _, R}) ->
    2*math:pi()*R;
perimeter({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    {A,B,C} = segment_lengths({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}),
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
%% triangle, the parallel side will go through the last point of the triangle (call it CD)
%% that was not in the base. We then find one of the vertices by taking the
%% line perpendicular to the base and find the intersection with CD.
enclose({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    {A,B,C} = segment_lengths({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}),
    {L, {X1, Y1}, {X2, Y2}} = extremal_length([A,B,C]),
    {Slope_base, Perp_slope} = calculate_slopes({X1, Y1}, {X2, Y2}),
    [{Last_x, Last_y}] = [{XP1, YP1}, {XP2, YP2}, {XP3, YP3}] -- [{X1, Y1}, {X2, Y2}],
    {XV, YV} = opposite_vertex({Last_x, Last_y}, {X1, Y1}, Slope_base, Perp_slope),
    {rectangle, {(XV + X2)/2, (YV + Y2)/2}, L, segment_length({X1, Y1}, {XV, YV})}.

%% The case where one of the lines is of the form x = c (for some constant c)
%% is an exceptional case, thus we are pattern matching against these specifically.
opposite_vertex({Last_x, _Last_y}, {_X1, Y1}, inf, _) ->
    {Last_x, Y1};
opposite_vertex({_Last_x, Last_y}, {X1, _Y1}, 0, _) ->
    {X1, Last_y};
opposite_vertex({Last_x, Last_y}, {X1, Y1}, Slope_base, Perp_slope) ->
    Parallel_intercept = Last_y - Slope_base*Last_x,
    Perp_y_intercept = Y1 - Perp_slope*X1,
    Non_trivial_vertex = (Perp_y_intercept - Parallel_intercept)/(Slope_base - Perp_slope),
    {Non_trivial_vertex, Slope_base*Non_trivial_vertex + Parallel_intercept}.

calculate_slopes({X1, _Y1}, {X2, _Y2}) when X1 == X2 ->
    {inf, 0};
calculate_slopes({_X1, Y1}, {_X2, Y2}) when Y1 == Y2 ->
    {0, inf};
calculate_slopes({X1, Y1}, {X2, Y2}) ->
    Slope_base = (Y2 - Y1)/(X2 - X1),
    Perp_slope = -1/(Slope_base),
    {Slope_base, Perp_slope}.


extremal_length(Lengths) ->
    lists:foldl(fun augment_max/2, {0, {0,0}, {0,0}}, Lengths).

augment_max({L1, {X1, Y1}, {X2, Y2}}, {L2, {_X3, _Y3}, {_X4, _Y4}}) when L1 > L2 ->
	{L1, {X1, Y1}, {X2, Y2}};
augment_max({L1, {_X1, _Y1}, {_X2, _Y2}}, {L2, {X3, Y3}, {X4, Y4}}) when L2 > L1 ->
	{L2, {X3, Y3}, {X4, Y4}}.
