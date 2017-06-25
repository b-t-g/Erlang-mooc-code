-module(ex).
-export([bits/1, perimeter/1, area/1, enclose/1]).

%% For representing a triangle, I chose the representation to just be the three
%% points defining the vertices; it both seemed like the most natural
%% representation as well as making it the easiest to find the smallest
%% enclosing rectangle.

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

%% Heron's formula (described earlier in the course)
area({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    A = segment_length(XP1, YP1, XP2, YP2),	
    B = segment_length(XP2, YP2, XP3, YP3),
    C = segment_length(XP3, YP3, XP1, YP1),
    S = (A + B + C)/2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

perimeter({rectangle, _, H, W}) ->
    2*(H + W);
perimeter({circle, _, R}) ->
    2*math:pi()*R;
perimeter({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    A = segment_length(XP1, YP1, XP2, YP2),
    B = segment_length(XP2, YP2, XP3, YP3),
    C = segment_length(XP3, YP3, XP1, YP1),
    A + B + C.

segment_length(X1, Y1, X2, Y2) ->
    math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

%% Added just for the sake of completeness.
enclose({rectangle, X, Y, Z}) ->
    {rectangle, X, Y, Z};
%% The smallest rectangle enclosing a circle is just a rectangle, centered
%% at the same location with widths and heights twice as big as the circle's radius.
enclose({circle, {X, Y}, R}) ->
    {rectangle, {X, Y}, 2*R, 2*R};

enclose({triangle, {XP1, YP1}, {XP2, YP2}, {XP3, YP3}}) ->
    A = {segment_length(XP1, YP1, XP2, YP2), {XP1, YP1}, {XP2, YP2}},
    B = {segment_length(XP2, YP2, XP3, YP3), {XP2, YP2}, {XP3, YP3}},
    C = {segment_length(XP3, YP3, XP1, YP1), {XP3, YP3}, {XP1, YP1}},
    {L, {X1, Y1}, {X2, Y2}} = find_extremal_length([A,B,C]),
    {Slope_base, Perp_slope} = calculate_slopes({X1, Y1}, {X2, Y2}),
    [{Last_x, Last_y}] = [{XP1, YP1}, {XP2, YP2}, {XP3, YP3}] -- [{X1, Y1}, {X2, Y2}],
    %% What to do when the slope of the longest segment is inf?
    Parallel_intercept = Last_y - Slope_base*Last_x,
    Perp_y_intercept = Y1 - Perp_slope*X1,
    Non_trivial_vertex = (Perp_y_intercept - Parallel_intercept)/(Slope_base - Perp_slope),
    {XV, YV} = {Non_trivial_vertex, Slope_base*Non_trivial_vertex + Parallel_intercept},
    {rectangle, {(XV + X2)/2, (YV + Y2)/2}, L, segment_length(X1, Y1, XV, YV)}.

calculate_slopes({X1, Y1}, {X2, Y2}) ->
    Slope_base = case X2 - X1 of
	     0 -> inf;
	     _ -> (Y2 - Y1)/(X2 - X1)
	 end,
    Perp_slope = case Slope_base of
		    inf -> 0;
		    _   -> -1/(Slope_base)
		end,
    {Slope_base, Perp_slope}.

    
find_extremal_length(Lengths) ->
    lists:foldl(fun augment_max/2, {0, {0,0}, {0,0}}, Lengths).

augment_max({L1, {X1, Y1}, {X2, Y2}}, {L2, {X3, Y3}, {X4, Y4}}) ->
    case max(L1, L2) of
	L1 -> {L1, {X1, Y1}, {X2, Y2}};
	L2 -> {L2, {X3, Y3}, {X4, Y4}}
    end.
