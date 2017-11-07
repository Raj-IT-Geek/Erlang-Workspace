-module(casetest).
-export([do/1]).

do(Bool) ->
  case (Bool andalso print()) of
    true -> ok;
    false -> not_ok
  end.

print() ->
  io:format("Printing ~n"),
  true.
