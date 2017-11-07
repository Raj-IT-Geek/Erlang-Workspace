-module(rec_map).
-compile(export_all).

-record(first, {one, two, three}).
-record(second, {one, two, three}).

rec_to_map(RecSpecs, RecTupleList) when is_list(RecTupleList) ->
  [ rec_to_map(RecSpecs, RecTuple) || RecTuple <- RecTupleList ];
rec_to_map([_RecName|Fields], RecTuple) ->
  [_|Values] = tuple_to_list(RecTuple),
  rec_to_map2(Fields, Values, []).

rec_to_map2([], _, Zip) ->
  maps:from_list(Zip);
rec_to_map2([{Fh, FhFields}|Fields], [ValueRec|Values], Zip) ->
  rec_to_map2(Fields, Values, [{Fh, rec_to_map(FhFields, ValueRec)}|Zip]);
rec_to_map2([Field|Fields], [Value|Values], Zip) ->
  rec_to_map2(Fields, Values, [{Field, Value}|Zip]).

map_to_rec(RecSpecs, List) when is_list(List) ->
  [map_to_rec(RecSpecs, Map) || Map <- List];
map_to_rec([RecName|Fields], Map) ->
  KvList = maps:to_list(Map),
  RecList = lists:foldl(
              fun
                ({Field, RecSpecs}, List) ->
                  {Field, Value} = lists:keyfind(Field, 1, KvList),
                  [map_to_rec(RecSpecs, Value)|List];
                (Field, List) ->
                  {Field, Value} = lists:keyfind(Field, 1, KvList),
                  [Value|List]
              end,
              [RecName],
              Fields
             ),
  list_to_tuple(lists:reverse(RecList)).


