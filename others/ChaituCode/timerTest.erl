-module(timerTest).

-export([start/3, runner/3]).

start(T, C, ST) ->
  fork_all(T, C, ST),
  All = receive_all(T * C, []),
  io:format("List follows~n~p~n", [All]),
  [First|AllS] = lists:sort(All),
  lists:foldl(
    fun(I, {P, D}) ->
        Dn = abs(I - P),
        if Dn < D -> {I, Dn}; true -> {I, D} end
    end,
    {First, 10000000000000000},
    AllS
   ).

fork_all(T, C, ST) ->
  Pid = self(),
  CS = lists:seq(1, C),
  lists:foreach(
    fun(_X) ->
        spawn(?MODULE, runner, [CS, ST, Pid])
    end,
    lists:seq(1, T)
   ).

runner(CS, ST, Pid) ->
  Now = erlang:system_time(),
  if
    Now > ST ->
      runner(CS, Pid);
    true ->
      runner(CS, ST, Pid)
  end.

runner(CS, Pid) ->
  TSs = [ os:perf_counter() || _X <- CS ],
  lists:foreach(fun(TS) -> Pid ! TS end, TSs).

receive_all(0, All) ->
  All;
receive_all(C, All) ->
  receive
    X ->
      receive_all(C - 1, [X|All])
  end.
