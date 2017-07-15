-module(palindrome).
-export([palindrome/1]).

-spec palindrome([integer()]) -> boolean().
palindrome([]) ->
    true;
palindrome([X]) ->
    true;
palindrome([X | Xs] = L) ->
    %% Only take numbers which represent letters
    Filtered = lists:filter(fun(Elem) -> Elem >= 65 andalso Elem =< 122 end, L),
    %% If a number is upper case, make it lower case; otherwise, do nothing.
    All_lower = lists:filtermap(fun(Elem) -> case  Elem < 97 of
						 true  -> {true, Elem + 32};
						 false -> true
					     end
				end, Filtered),
   All_lower == reverse(All_lower). 

-spec reverse([T]) -> [T].
reverse([]) ->
    [];
reverse(Xs) ->
    move(Xs, []).

-spec move([T], [T]) -> [T].
move([], Ys) ->
    Ys;
move([X | Xs], Ys) ->
    move(Xs, [X | Ys]).

