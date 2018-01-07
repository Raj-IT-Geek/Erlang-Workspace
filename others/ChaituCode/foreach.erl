-module(foreach).
-compile(export_all).

oof(NamesList) ->
  lists:foreach(
    fun print/1,
    NamesList
   ).

print(Name) ->
  io:format("~p~n", [Name]).
