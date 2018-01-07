-module(fibonacci).
-export([within/1,
         within/2,
         sum/1,
         sum/2]).

sum(Limit) ->
  lists:sum(within(Limit)).

sum(Limit, {A, B}) ->
  lists:sum(within(Limit, {A, B})).

within(Limit) ->
  within(Limit, {0, 1}).

within(Limit, {A, B}) ->
  within([B, A], Limit);
within([B|[A|R]], Limit) when B >= Limit ->
  lists:reverse([A|R]);
within([B|[A|_R]] = Fib, Limit) ->
  within([B+A|Fib], Limit).
