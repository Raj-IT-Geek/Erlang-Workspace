-module(solution).
-export([main/0]).

main() ->
  TCs = get_number(),
  main(TCs).

main(0) -> ok;
main(TCs) ->
  String = get_string(),
  rotate(String),
  main(TCs - 1).

rotate(String) ->
  rotate(String, length(String)).

rotate(_Sr, 0) ->
  io:format("~n");
rotate([H|T], L) ->
  Sr = T ++ [H],
  io:format("~s ", [Sr]),
  rotate(Sr, L -1).

get_number() ->
  {ok, [N]} = io:fread("", "~d"),
  N.

get_string() ->
  string:strip(io:get_line(""), right, 10).
