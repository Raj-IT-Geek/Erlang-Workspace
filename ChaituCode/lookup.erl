-module(lookup).
-export([init/1, lookup/1]).

init(N) ->
  mnesia:create_table(lookup, [{attributes, [did, queue]}]),
  load(N).

load(N) ->
  lists:foreach(
    fun(X) ->
        Did = integer_to_list(9000000000 + X),
        Queue = pick_queue(),
        mnesia:dirty_write({lookup, Did, Queue})
    end,
    lists:seq(1, N)
   ).

lookup(Did) ->
  case mnesia:dirty_read(lookup, Did) of
    [] ->
      pick_queue();
    [{lookup, Did, Queue}] ->
      Queue
  end.


pick_queue() ->
  element(rand:uniform(10),
          {one, two, three, four, five,
           six, seven, eight, nine, ten}).

