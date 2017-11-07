-module(add).
-export([sum/1]).

sum(N) ->
  sum(1, N, 0).

sum(I, N, S) when I > N -> S;
sum(I, N, S) ->
  sum(I + 1, N, S + I).

% sum(0) -> 0;
% sum(N) ->
%   N + sum(N-1).
