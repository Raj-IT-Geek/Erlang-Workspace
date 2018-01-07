%%% Module to reverse a given string.

-module(reverse).
-export([rev/1]).

rev(Str) -> rev(Str,[]).

rev([],RevStr) -> RevStr;

rev([H|T],RevStr) ->
    io:format("\nH=~c, T=~p, RevStr=~p\n", [H, T, RevStr]),
    rev(T, [H|RevStr]).
