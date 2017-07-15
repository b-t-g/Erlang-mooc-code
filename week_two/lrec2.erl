-module(lrec2).
-export([double/1, evens/1, median/1, modes/1]).

double([X | Xs]) ->
    [2 * X | double(Xs)];
double([]) ->
    [].

evens([X | Xs]) when X rem 2 == 0 ->
    [X | evens(Xs)]; 
evens([_X | Xs]) ->
    evens(Xs); 
evens([]) ->
    [].

median([X]) ->
    X;
median(L) ->
    Sorted = lists:sort(L),
    case (Length = length(L)) rem 2 of
	0 ->
	    (lists:nth(Length div 2, Sorted) + lists:nth((Length div 2) + 1, Sorted))/2;
	1 ->
	    lists:nth((Length + 1) div 2, Sorted)
    end.

modes([X]) ->
    X;
modes(L) ->
    Aggregate = lists:foldl(fun(Val, Acc) -> increment(Val, Acc) end, #{}, L),
    Max = lists:max(maps:values(Aggregate)),
    maps:fold(fun(K, V, Acc) -> case V of
				    Max -> [K | Acc];
				    _   -> Acc
				end
	      end, [], Aggregate).

increment(Val, Map) ->
    maps:put(Val, maps:get(Val, Map, 0) + 1, Map).



    


