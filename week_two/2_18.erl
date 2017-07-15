-module('2_18').
-export([join/2, concat/1, member/2, merge_sort/1, insertion_sort/1, quick_sort/1, perms/1]).

-spec join([T], [T]) -> [T].
join([], Y) ->
    Y;
join(X, []) ->
    X;
join([X | Xs], Y) ->
    [X | join(Xs, Y)].

-spec concat([T]) -> T.
concat([]) ->
    [];
concat([X]) ->
    X;
concat([X, Y | Xs]) ->
    join(join(X, Y), concat(Xs)).

-spec member(T, [T]) -> boolean().
member(_Elem, []) ->
    false;
member(Elem, [X | Xs]) ->
    (Elem =:= X) orelse member(Elem, Xs).

-spec merge_sort([T]) -> [T].
merge_sort([X, Y]) when X >= Y ->
    [Y, X];
merge_sort([X, Y]) when X =< Y ->
    [X, Y];
merge_sort([X]) ->
    [X];
merge_sort(X) ->
    Length = length(X),
    Parity = length(X) rem 2,
    First_half = lists:sublist(X, Length div 2),
    Second_half = lists:sublist(X, (Length div 2) + 1, Length + Parity),
    merge(merge_sort(First_half), merge_sort(Second_half)).

-spec merge([T], [T]) -> [T]. 
merge([], []) ->
    [];
merge([], Y) ->
    Y;
merge(X, []) ->
    X;
merge([X | Xs] = L, [Y | Ys]) when X >= Y ->
    [Y | merge(L, Ys)];
merge([X | Xs], [Y | Ys] = L) when X < Y->
    [X | merge(Xs, L)].

-spec insertion_sort([T]) -> [T].
insertion_sort([X | Xs]) ->
    merge([X], insertion_sort(Xs));
insertion_sort([]) ->
    [].

-spec quick_sort([T]) -> [T].
quick_sort([X]) ->
    [X];
quick_sort([X, Y]) when X =< Y ->
    [X, Y];
quick_sort([X, Y]) when Y =< X ->
    [Y, X];
quick_sort(L) ->
    Pivot_index = rand:uniform(length(L)),
    Pivot = lists:nth(Pivot_index, L),
    Lower_half = [X || X <- L, X =< Pivot],
    Upper_half = [X || X <- L, X > Pivot],
    Lower_half_sorted = quick_sort(Lower_half),
    Upper_half_sorted = quick_sort(Upper_half),
    join(Lower_half_sorted, Upper_half_sorted).

-spec perms([T]) -> [[T]].
perms([X]) ->
    [[X]];
perms([X, Y]) ->
    [[X, Y], [Y, X]];
perms([X | Xs] = L) ->
    Perms = perms(Xs),
    Indices = lists:seq(0, length(Xs)),
    %% For each possible index, split the list into a prefix and suffix,
    %% prepend X to the suffix, combine the prefix and suffix, then
    %% flatten the resulting list.
    lists:flatmap(fun(Permutation) ->
		      lists:map(fun(Y) -> {Prefix, Suffix} = lists:split(Y, Permutation),
				Prefix ++ [X | Suffix]
				end, Indices)
	      end, Perms).
