% Demo of importing a function from another module.
-module(importhello).
-import(hello, [start/0]).
-export([greet/0]).

greet() ->
  start().

