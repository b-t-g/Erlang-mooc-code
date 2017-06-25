-module(recursion).
-export([fib/1, fib_rec/1, pieces/1, perfect/1, perfect_recursive/1]).

% This is correct, but quite slow (even for N = 50, I don't even know whether it will ever finish!)
fib_rec(0) ->
    0;
fib_rec(1) ->
    1;
fib_rec(N) ->
    fib_rec(N-1) + fib_rec(N-2).

% An iterative approach, which is much faster.
fib(0) ->
    0;
fib(1) ->
    1;
fib(N) ->
    fib_help(1, 1, N - 2).

fib_help(X,Y,Remaining) when Remaining > 0 ->
    fib_help(Y, X+Y, Remaining - 1);
fib_help(_, Y, Remaining) when Remaining == 0 ->
    Y.

% This was much harder for me to prove than to write. The key observation is that
% a new piece is created not only when your new line intersects another, but
% when your new line "enters" a line AND when it "exits", which leads to that
% "+ N" in the general case.
pieces(1) ->
    2;
pieces(2) ->
    4;
pieces(N) ->
    pieces(N-1) + N.

perfect(N) ->
    Potential_divisors = lists:seq(1, trunc(N/2)),
    N == lists:foldr(fun(Elem, Acc) -> case N rem Elem of
					    0 -> Acc + Elem;
					    _ -> Acc
					end
		      end, 0, Potential_divisors).

perfect_recursive(N) when N == 1 ->
    true;
perfect_recursive(N) ->
    N == perfect_help(trunc(N/2), 0, N).

perfect_help(Step, Acc, N) when Step == 1 ->
    Acc + 1;
perfect_help(Step, Acc, N) ->
    case N rem Step of
	0 -> perfect_help(Step - 1, Acc + Step, N);
	_ -> perfect_help(Step - 1, Acc, N)
    end.
