-module(pattern).
-export([match/1, help/0, match2/1,
         check/1, hello/1]).

match({circle, R}) ->
  Rsquare = R * R,
  3.1415 * Rsquare;
match({square, S}) ->
  S * S;
match({rectangle, S, S}) ->
  match({square, S});
match({rectangle, L, B}) ->
  L * B.

help() ->
  "I don't help".

match2(Param) ->
  case Param of
    {circle, R} ->
      3.1415 * R * R;
    {square, S} ->
      S * S;
    {rectangle, L, B} ->
      L * B;
    true ->
      "not param"
  end.

check(Age) ->
  if
    Age < 18 ->
      no_beer;
    true ->
      yes_beer
  end.

hello(Name) ->
  timer:sleep(2000),
  io:format("Hello, ~s~n", [Name]).
