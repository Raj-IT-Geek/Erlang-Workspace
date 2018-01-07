-module(utils_test).
-compile(export_all).

rec_to_map(Fields, RecTuple) ->
    [_|Values] = tuple_to_list(RecTuple),
    rec_to_map2(Fields, Values, []).

rec_to_map2([], _, Zip) ->
    maps:from_list(Zip);
rec_to_map2([{Fh, FhFields}|Fields], [Vh|Values], Zip) ->
    rec_to_map2(Fields, Values, [{Fh, rec_to_map(FhFields, Vh)}|Zip]);
rec_to_map2([[{Fh, FhFields}]|Fields], [Vh|Values], Zip) ->
  rec_to_map2(Fields, Values,
              [{Fh, [rec_to_map(FhFields, Vh1) || Vh1 <- Vh]}|Zip]);
rec_to_map2([Fh|Fields], [Vh|Values], Zip) ->
  rec_to_map2(Fields, Values, [{Fh, Vh}|Zip]).


map_to_rec(RecName, Map) ->
  map_to_rec2(RecName, maps:to_list(Map)).

map_to_rec2(RecName, ValueList) ->
  
