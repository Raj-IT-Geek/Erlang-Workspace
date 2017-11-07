-module(mnesiaSelect).
-export([create/0,
         store/3,
         get/2]).

-record(test, {no, name, city}).

create() ->
  mnesia:create_table(test, [{attributes, record_info(fields, test)},
                              {index, [name, city]}]).

store(No, Name, City) ->
  mnesia:dirty_write(#test{no=No,
                           name=Name,
                           city=City}).

get(No, Selection) ->
  [Rec] = mnesia:dirty_read(test, No),
  io:format("Rec is ~p~n", [Rec]),
  lists:map(fun(Select) -> element(Select, Rec) end,
            Selection).
