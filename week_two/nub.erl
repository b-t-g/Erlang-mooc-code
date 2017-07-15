-module(nub).
-export([nub/1]).

nub([]) ->
    [];

nub([X | Xs] = L) ->
    nub(L, #{}).

nub([], Map) ->
    [];
nub([X], Map) ->
    case maps:get(X, Map, 0) of
	0 -> [X];
	1 -> []
    end;
nub([X | Xs], Map) ->
    case maps:get(X, Map, 0) of
	1 -> nub(Xs, Map);
	0 -> [X | nub(Xs, maps:put(X, 1, Map))]
    end.
