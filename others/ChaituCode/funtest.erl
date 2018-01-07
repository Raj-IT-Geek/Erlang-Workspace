-module(funtest).
-export([make/1]).

make(NamesList) ->
  lists:map(fun print/2(10), NamesList).

print(_X, Name) ->
  io:format("Hey I am here ~p~n", [Name]).
