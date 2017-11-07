-module(party_counter).

-define(COUNTERS, [male, female, kid]).

-export([start/0, enter/1, leave/1, count/0]).

start() ->
  lists:foreach( fun(Cname) ->
                     register(Cname, spawn(fun counter/0))
                 end,
                 ?COUNTERS ).

enter(Cname) when Cname == male; Cname == female; Cname == kid ->
    Cname ! entry;
enter(_Other) -> not_allowed.

leave(Cname) when Cname == male; Cname == female; Cname == kid ->
    Cname ! exit;
leave(_Other) -> not_allowed.

count() ->
  lists:map(
    fun get_count/1,
    ?COUNTERS
   ).

get_count(Cname) ->
    Cname ! {count, self()},
    receive
        Count ->
            {Cname, Count}
    end.

counter() -> counter(0).

counter(Count) ->
    receive
        entry ->
            counter(Count + 1);
        exit ->
            counter(Count - 1);
        {count, Pid} ->
            Pid ! Count,
            counter(Count)
    end.

