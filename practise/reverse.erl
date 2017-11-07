%%% Module to reverse a given string.

-module(reverse).
-export([reverse/1]).

reverse(Str) -> reverse(Str,[]).

reverse([],RevStr) -> RevStr;
reverse([H|T],RevStr) -> reverse(T, [H|RevStr]).
