-module(parallel_lanes).
-export([start/1, lane/1]).

start(NoOfLanes) ->
  [spawn(parallel_lanes, lane, [L]) || L <- lists:seq(1, NoOfLanes)].

lane(L) ->
  lane(L, 10).

lane(_L, 0) -> dubbe;
lane(L, N) ->
  timer:sleep(2000),
  io:format("Lane:~p~n", [L]),
  lane(L, N -1).

