-module(mylists).
-export([max_list/1]).

max_list([Head|Rest]) ->
  find_max(Rest, Head).

find_max([], Max) -> Max;
find_max([Head|Rest], Max) when Head > Max ->
  find_max(Rest, Head);
find_max([_Head|Rest], Max) ->
  find_max(Rest, Max).
