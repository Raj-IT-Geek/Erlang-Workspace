-module(watchman).
-export([start/0, enter/0, leave/0, count/0, close/0]).

-export([gatekeeper/1]).

start() ->
  register(watchman, spawn(?MODULE, gatekeeper, [0])).

gatekeeper(Counter) ->
  receive
    enter ->
      CounterNew = Counter + 1,
      gatekeeper(CounterNew);
    leave ->
      CounterNew = Counter - 1,
      gatekeeper(CounterNew);
    count ->
      io:format("Count is ~p~n", [Counter]),
      gatekeeper(Counter);
    close ->
      io:format("Now closing ~n")
  end.

enter() ->
  watchman ! enter.

leave() ->
  watchman ! leave.

count() ->
  watchman ! count.

close() ->
  watchman ! close.
