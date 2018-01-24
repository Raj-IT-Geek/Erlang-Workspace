-module(person_db_3_new).
-export([init/0, add/2, all_names/0, delete/1, find/1, handle/2]).
-import(server_3, [rpc/2]).

%% interface
all_names()      -> rpc(person_db_server, all_names).
add(Person_name, Place) -> rpc(person_db_server, {add, Person_name, Place}).
delete(Person_name)     -> rpc(person_db_server, {delete, Person_name}).
find(Person_name)       -> rpc(person_db_server, {find, Person_name}).

% callback routines
init() -> dict:new().
handle({add, Person_name, Place}, Dict) -> {ok, dict:store(Person_name, Place, Dict)};
handle(all_names, Dict)           -> {dict:fetch_keys(Dict), Dict};
handle({delete, Person_name}, Dict)     -> {ok, dict:erase(Person_name, Dict)};
handle({find, Person_name}, Dict)       -> {dict:find(Person_name, Dict), Dict}.
