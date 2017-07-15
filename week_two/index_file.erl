-module(index_file).
-import(index, [get_file_contents/1]).
-export([main/1, compress_list/1]).

-spec main(string()) -> [{string(), integer(), integer()}].
main(File_handle) ->
    Contents = lists:map(fun(Line) -> all_lower_case(Line) end,
			 get_file_contents(File_handle)),
    Split = lists:map(fun(Str) -> lists:filter(fun(X) -> length(X) > 0 end,
					       string:split(Str, " ", all)) end,
		      Contents),
    Word_count = add_word_occurrence(Split),
    Formatted_list = maps:to_list(maps:map(fun(_K, V) -> compress_list(V) end, Word_count)),
    lists:sort(fun({A, _Xs}, {B, _Ys}) -> A =< B end, 
	      lists:filter(fun({A, _Xs}) -> length(A) > 2 end, Formatted_list)).
    

-spec add_word_occurrence([string()]) -> #{string() => [integer()]}.
add_word_occurrence(Lines) ->
    add_word_occurrence(Lines, 1, #{}).

%% Add the line number to each word in a line.
%% ex. add_word_occurrence(["foo", "bar", "baz"], 3) -> #{"foo" -> [3], "bar" -> [3], "baz" -> [3]}
add_word_occurrence([Line | Rest], Line_num, Acc) ->
    add_word_occurrence(Rest, Line_num + 1,
	lists:foldl(fun(Word, Map) ->
			    Current = maps:get(Word, Map, []),
			    New = case Current =:= [] orelse Line_num /= lists:nth(1, Current) of
				      true -> [Line_num | Current];
				      false  -> Current
				  end,
			    maps:put(Word, New, Map)
		    end, Acc, Line));
add_word_occurrence([], _Line_num, Acc) ->
    Acc.

-spec all_lower_case(string()) -> string().
all_lower_case(L) ->
    %% Only take numbers which represent letters
    Filtered = lists:filter(fun is_letter/1, L),
    %% If a number is upper case, make it lower case; otherwise, do nothing.
    lists:filtermap(fun(Elem) -> case Elem < 97 andalso Elem /= 32 of
				     true  -> {true, Elem + 32};
				     false -> true
				 end
		    end, Filtered).
is_letter(Elem) ->
    Elem == 32 orelse Elem >= 65 andalso Elem =< 90 orelse
	Elem >= 97 andalso Elem =< 122.

%% Convert a list of line numbers into the correct format.
%% ex: index_file:compress_list([10, 9, 8, 5, 4, 1]) -> [{1, 1}, {4, 5}, {8, 10}]
-spec compress_list([integer()]) -> [{integer(), integer()}].
compress_list(L) ->
    lists:foldl(fun accumulate/2, [], L).
    
%% Take a number and a list of integer pairs, if the incoming number is one less
%% than the most recently encountered lowest number, then replace the entry with
%% the incoming number as the lower bound, otherwise, create a new entry with
%% that number as both the upper and lower bound.
%%
%% This function assumes that there are no duplicates in the list and are
%% integers in descending order, (they are filtered out in add_word_occurrences)
-spec accumulate(integer, [{integer(), integer()}]) -> [{integer(), integer()}].
accumulate(Line_num, []) ->
    [{Line_num, Line_num}];
accumulate(Line_num, [X | Xs] = Acc) ->
    [X | Xs] = Acc,
    {Lower, Upper} = X,
    case Lower - Line_num of
	1 -> [{Line_num, Upper} | Xs];
	_ -> [{Line_num, Line_num}, X | Xs]
    end.
