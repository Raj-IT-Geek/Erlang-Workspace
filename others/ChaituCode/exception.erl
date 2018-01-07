-module(exception).
-export([run/2]).

run({M, F, A}, D) ->
  case (catch apply(M, F, A)) of
    {'EXIT', _Reason} ->
      D;
    R ->
      R
  end;
run({F, A}, D) when is_function(F) ->
  case (catch apply(F, A)) of
    {'EXIT', _Reason} ->
      D;
    R ->
      R
  end;
run({F, A}, D) ->
  run({erlang, F, A}, D).
