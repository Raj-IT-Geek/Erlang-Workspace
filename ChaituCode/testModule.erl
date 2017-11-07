-module(testModule).
-export([ask/0, ask/1, greet/0]).

ask() ->
  16.

ask(Age) ->
  Age - 10.

greet() ->
  "Hello".
