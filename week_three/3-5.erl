-module('3-5').
-export([doubleAll/1, evens/1, product/1, zip/2, zipWith/3, zipWithMap/3, zipUsingZW/2]).

doubleAll(L) ->
     lists:map(fun(X) -> 2*X end, L).
evens(L) ->
    lists:filter(fun(X) -> X rem 2 == 0 end, L).
product(L) ->
    lists:foldl(fun(X, Acc) -> X*Acc end, 1, L).

zip(X, Y) when X =:= []; Y =:= [] ->
    [];
zip([X], [Y]) ->
    [{X, Y}];
zip([X | _], [Y]) ->
    [{X, Y}];
zip([X], [Y | _]) ->
    [{X, Y}];
zip([X | Xs], [Y | Ys]) ->
    [{X, Y} | zip(Xs, Ys)].

zipWith(_, X, Y) when X =:= []; Y =:= [] ->
    [];
zipWith(Fun, [X], [Y]) ->
    [Fun(X, Y)];
zipWith(Fun, [X | _], [Y]) ->
    [Fun(X, Y)];
zipWith(Fun, [X], [Y | _]) ->
    [Fun(X, Y)];
zipWith(Fun, [X | Xs], [Y | Ys]) ->
    [Fun(X, Y) | zipWith(Fun, Xs, Ys)].
    
zipWithMap(Fun, X, Y) ->
    lists:map(fun({Z,W}) -> Fun(Z,W) end, zip(X, Y)).

zipUsingZW(X, Y) ->
    zipWith(fun(Z, W) -> {Z, W} end, X, Y).
