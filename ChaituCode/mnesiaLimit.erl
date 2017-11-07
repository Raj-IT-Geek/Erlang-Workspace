-module(mnesiaLimit).

-export([create/1]).

create(N) ->
  lists:foreach(
    fun(I) ->
        T = list_to_atom("table_" ++ integer_to_list(I)),
        mnesia:create_table(T, [])
    end,
    lists:seq(1, N)
   ).
