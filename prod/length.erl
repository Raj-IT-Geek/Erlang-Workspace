-module(tail).
-export([length/1]).

length(List) -> length(List,0).

length([], Len) -> Len;
length( [ _ | TailOfList ], Len) ->
    io:format("Tail: ~p \t Len:~p ~n", [TailOfList, Len]),
    length(TailOfList, Len+1).
