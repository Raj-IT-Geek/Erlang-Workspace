-module(mnesiaKeys).
-export([is_key/2, are_all_keys/2]).

is_key(Tname, Key) ->
  case catch ets:member(Tname, Key) of
    {'EXIT', _Reason} ->
      is_remote_key(Tname, Key);
    Boolean -> Boolean
  end.

is_remote_key(Tname, Key) ->
  case mnesia:dirty_read(Tname, Key) of
    [] -> false;
    _ -> true
  end.

are_all_keys(Tname, Keys) ->
  Fun = case mnesia:table_info(Tname, storage_type) of
          unknown -> fun is_remote_key/2;
          _ -> fun is_key/2
        end,
  are_all_keys(Tname, Keys, Fun).

are_all_keys(_Tname, [], _Fun) -> true;
are_all_keys(Tname, [Key|Keys], Fun) ->
  case Fun(Tname, Key) of
    false -> false;
    true -> are_all_keys(Tname, Keys, Fun)
  end.

is_key2(Tname, Key) ->
  case 
