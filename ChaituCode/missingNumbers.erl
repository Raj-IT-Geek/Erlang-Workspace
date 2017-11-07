-module(missingNumbers).
-export([main/0,
         find/2]).

main() ->
  ListA = get_numbers(),
  ListB = get_numbers(),
  find(ListA, ListB).

get_numbers() ->
  {ok, [N]} = io:fread("", "~d"),
  lists:map(
    fun(_I) ->
        {ok, [X]} = io:fread("", "~d"),
        X
    end,
    lists:seq(1, N)
   ).

find(ListA, ListB) ->
  find(ListA, ListB, []).

find([H|ListA], [H|ListB], Missing) ->
  find(ListA, ListB, Missing);
find(ListA, [Hb|ListB], Missing) ->
  case lists:member(Hb, Missing) of
    false ->
      find(ListA, ListB, [Hb|Missing]);
    true ->
      find(ListA, ListB, Missing)
  end;
find(_ListA, [], Missing) ->
  lists:reverse(Missing).
