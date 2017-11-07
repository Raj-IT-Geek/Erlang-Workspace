-module(mnesiaTrans).

-export([create/2,
         add/1,
         list/1]).

create(Tname, Options) ->
  case lists:member(Tname, mnesia:system_info(local_tables)) of
    false ->
      mnesia:create_table(Tname, Options);
    true ->
      {attributes, FieldsNew} = lists:keyfind(attributes, 1, Options),
      FieldsOld = mnesia:table_info(test, attributes),
      Transformer = make_transformer(Tname, FieldsOld, FieldsNew),
      mnesia:transform_table(Tname, Transformer, FieldsNew)
  end.

add(Object) ->
  mnesia:dirty_write(Object).

list(Tname) ->
  [ mnesia:dirty_read(Tname, Key) || Key <- mnesia:dirty_all_keys(Tname) ].

make_transformer(Tname, OldFields, NewFields) ->
  fun(OldRec) ->
      list_to_tuple([Tname|lists:map(
        fun(NewField) ->
            case string:str(OldFields, [NewField]) of
              0 -> undefined;
              Pos -> element(Pos + 1, OldRec)
            end
        end,
        NewFields
       )])
  end.
