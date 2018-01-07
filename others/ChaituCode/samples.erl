-module(samples).
-compile(export_all).

pair(Husbands, Wives) ->
  pair(Husbands, Wives, []).

pair([Husband|OtherHs], [Wife|OtherWs], Pair) ->
  pair(OtherHs, OtherWs, [{Husband, Wife}|Pair]);
pair([], [], Pair) ->
  Pair.

hello(Name) ->
  case Name of
    "danny" ->
      io:format("Hey me ~n");
    Other ->
      io:format("Hey ~s ~n", [Other])
  end.
