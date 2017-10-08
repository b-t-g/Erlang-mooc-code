-module(rps).
-export([tournament/2]).

-spec beat(atom()) -> atom().
beat(rock) ->
    paper;
beat(paper) ->
    scissors;
beat(scissors) ->
    rock.
-spec lose(atom()) -> atom().
lose(rock) ->
    scissors;
lose(scissors) ->
    paper;
lose(paper) ->
    rock.

-spec result(atom(), atom()) -> integer().
result(X, X) ->
    0;
result(X, Y) ->
    case lose(X) =:= Y of
	true -> 1;
	false -> -1
    end.

-spec strategy(atom(), [atom()]) -> atom().
strategy(rock, _L) ->
    rock;
strategy(echo, [X | _]) ->
    X;
strategy() ->


-spec tournament([atom()], [atom()])-> integer().
tournament(Left, Right) ->
    lists:foldl(fun(X, Y) -> X + Y end, 0, lists:zipwith(fun result/2, Left, Right)).
