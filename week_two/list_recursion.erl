-module(list_recursion).
-export([prod/1, prod_iterative/1, max/1, max_iterative/1]).

prod([X|Xs]) ->
    X*prod(Xs);
prod([]) ->
    1.

prod_iterative(X) ->
    prod_iterative(X, 1).
prod_iterative([X|Xs], Acc) ->
    prod_iterative(Xs, X*Acc);
prod_iterative([], Acc) ->
    Acc.

max([X]) ->
    X;
max([X|Xs]) ->
    max(X, max(Xs)).

max_iterative([X|Xs]) ->
    max_iterative(Xs, X).
max_iterative([], Max) ->
    Max;
max_iterative([X|Xs], Max) ->
    max_iterative(Xs, max(Max, X)).

