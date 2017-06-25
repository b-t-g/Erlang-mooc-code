-module(simple)
-export([how_many_equal/1])
how_many_equal(X,Y,Z) ->
    min(3, 2*(3 - sets:size(sets:from_list([X,Y,Z])))).
exOr(X,X) ->
    false;
exOr(_,_) ->
    true.
