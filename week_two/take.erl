-module(take).
-export([take/2]).
-spec take(integer(), [T]) -> [T].
take(X, L) ->
    %% Presumably appending an element to the end of the list takes O(N)
    %% where N is the size of the list, thus reversing the list at the
    %% end makes this approach O(N) instead of appending the element at
    %% the end of each partial list L which has complexity O(length(L))
    %% which would be O(N^2).
    lists:reverse(take(X, L, [])).
-spec take(integer, [T], [T]) -> [T].
take(0, _, Acc) ->
    Acc;
take(_, [], Acc) ->
    Acc;
take(X, [Y | Ys], Acc) ->
    take(X - 1, Ys, [Y | Acc]).
