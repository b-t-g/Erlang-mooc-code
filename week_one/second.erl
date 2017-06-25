-module(second).
-import(first, [square/1]).
-export([hypotenuse/2, perimeter/2, area/2]).

hypotenuse(A,B) ->
   math:sqrt(first:square(A) + first:square(B)).

perimeter(A,B) ->
    A+B+hypotenuse(A,B).

area(A,B) ->
    first:mult(A,B)/2.
