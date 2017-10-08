-module(rps_course).
-export([play/1,echo/1,rock/1,no_repeat/1,enum/1,cycle/1,rand/1,val/1,tournament/2, lowest_frequency/1, highest_frequency/1, strategy_vs_strategy/3]).


%
% play one strategy against another, for N moves.
%

%% I didn't see the original skeleton functions, here's what I wrote
strategy_vs_strategy(Strat1, Strat2, Turns) ->
    strategy_vs_strategy(Strat1, Strat2, [], [], Turns).

strategy_vs_strategy(Strat1, Strat2, Plays1, Plays2, 0) ->
    lists:sum(lists:map(fun({Play1, Play2}) -> outcome(result(Play1, Play2)) end,
			lists:zip(Plays1, Plays2)));
strategy_vs_strategy(Strat1, Strat2, Plays1, Plays2, Turns) when Turns > 0 ->
    New_plays1 = [Strat1(Plays2) | Plays1],
    New_plays2 = [Strat2(Plays1) | Plays2],
    strategy_vs_strategy(Strat1, Strat2, New_plays1, New_plays2, Turns - 1).

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n"),
	    lists:foldl(fun({Outcome, _Play}, Acc) -> Outcome + Acc end, 0, Moves);
	_    ->
	    Result = result(Play,Strategy(Moves)),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[{outcome(Result), Play}|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

%
% strategies.
%
echo([]) ->
     paper;
echo([Last|_]) ->
    Last.

rock(_) ->
    rock.
% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

no_repeat([]) ->
    paper;
no_repeat([X|_]) ->
    beats(X).

cycle(Xs) ->
    case length(Xs) rem 3 of
	0 -> rock;
	1 -> paper;
	2 -> scissors
    end.

frequency([], _F) ->
    rock;
frequency(L, F) ->
    Counts = count(L),
    Compare = fun({Play1, Frequency1} = L1, {Play2, Frequency2} = L2) ->
		      case F(Frequency1, Frequency2) of
			  Frequency1 -> L1;
			  Frequency  -> L2
		      end
	      end,
    Starting_value = lists:nth(1, Counts),
    {Play, Frequency} = lists:foldl(Compare, Starting_value, Counts),
    beats(beats(Play)).
count([]) ->
    #{rock => 0, paper => 0, scissors => 0};
count(L) ->
    count(L, #{rock => 0, paper => 0, scissors => 0}).

count([], Map) ->
    maps:to_list(Map);
count([X | Xs], Map) ->
    Current = maps:get(X, Map),
    count(Xs, maps:put(X, Current + 1, Map)).

lowest_frequency(L) ->
    frequency(L, fun min/2).
highest_frequency(L) ->
    frequency(L, fun max/2).

rand(_) ->
    lists:nth(rand:uniform(3), [rock, paper, scissors]).

random_strategy(L, Opposing) ->
    Length = length(L),
    Strategy = lists:nth(rand:uniform(Length), L),
    Strategy(Opposing).


best_strategy(Strategies, Opposing) ->
    lists:max(lists:map(fun(Strategy) -> strategy_to_outcome(Strategy, Opposing) end, Strategies)).
    
strategy_to_outcome(Strategy, Opposing) when is_list(Opposing); length(Opposing) =< 1 ->
    0;
strategy_to_outcome(Strategy, [Most_recent, Second_most_recent | Rest] = Moves) ->
    lists:sum([outcome(result(Most_recent, Strategy([Second_most_recent | Rest]))) |
	      strategy_to_outcome(Strategy, [Second_most_recent | Rest])]).
