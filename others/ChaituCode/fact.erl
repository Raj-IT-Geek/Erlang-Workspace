-module(fact).
-export([oof/1]).

% oof(0) -> 1;
% oof(X) -> X * oof(X-1).

oof(X) -> poof(X, 1).

poof(0, F) -> F;
poof(X, F) ->
  io:format("X is ~p and F is ~p~n", [X, F]),
  poof(X-1, F * X).
