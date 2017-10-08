-module(hofs).
-export([compose/2, compose/1, double/1, iterate/1]).

-spec compose(fun((any()) -> T2), fun((T2) -> T3)) -> T3.
compose(F, G) ->
    fun(X) -> G(F(X)) end.

%% Spec'ing this out in full generality (and full safety) requires, I believe,
%% dependent types (or something similar) since the type of one function
%% depends on the type of the function preceding it.
-spec compose([fun()]) -> fun().
compose(L) when is_list(L) ->
    lists:foldl(fun compose/2, fun(X) -> X end, L).

-spec double(fun((T) -> T)) -> fun((T) -> T).
double(F) ->
    compose([F, F]).

iterate(N) ->
    fun(F) -> compose(lists:duplicate(N, F)) end.
