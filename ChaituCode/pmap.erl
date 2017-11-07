-module(pmap).
-export([pmap/2, run/3]).

pmap(Fun, List) ->
  Me = self(),
  PidD = lists:foldl(
           fun(X, PidDict) ->
               Pid = spawn(pmap, run, [Fun, X, Me]),
               dict:store(Pid, [], PidDict)
           end,
           dict:new(),
           List
          ),
  collect(PidD, []).

run(Fun, X, Me) ->
  Me ! {self(), Fun(X)}.

collect(PidD, ResList) ->
  case dict:is_empty(PidD) of
    false ->
      receive
        {Pid, Res} ->
          case dict:is_key(Pid, PidD) of
            true ->
              PidDU = dict:erase(Pid, PidD),
              collect(PidDU, [Res|ResList]);
            false ->
              collect(PidD, ResList)
          end
      after 30000 ->
              lists:reverse(ResList)
      end;
    true ->
      lists:reverse(ResList)
  end.
