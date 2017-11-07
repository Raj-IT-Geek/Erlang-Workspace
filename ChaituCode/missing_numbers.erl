-module(solution).

-export([main/0]).

main() ->
  io:fread("", "~d"),
  LineA = io:get_line(""),
  DictA = lists:foldl(
            fun(X, Dict) ->
                dict:update_counter(X, 1, Dict)
            end,
            dict:new(),
            [list_to_integer(X) || X <- string:tokens(LineA, " \n\r")]
           ),
  io:fread("", "~d"),
  LineB = io:get_line(""),
  {List, _} = lists:foldl(
             fun(X, {List, DictAn}) ->
                 case dict:find(X, DictAn) of
                   error ->
                     {update(X, List), DictAn};
                   {ok, 1} ->
                     {List, dict:erase(X, DictAn)};
                   {ok, _K} ->
                     {List, dict:update_counter(X, -1, DictAn)}
                 end
             end,
             {[], DictA},
             [list_to_integer(X) || X <- string:tokens(LineB, " \n\r")]
            ),
  [ io:format("~p ", [X]) ||
    X <- lists:sort(List) ],
  io:format("~n").

update(X, List) ->
  case lists:member(X, List) of
    true ->
      List;
    false ->
      List ++ [X]
  end.
