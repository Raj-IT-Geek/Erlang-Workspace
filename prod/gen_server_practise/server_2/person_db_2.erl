-module(person_db_2).
-export([init/0, add/2, find/1, handle/2]).
-import(server_3, [rpc/2]).

%% client routines
add(Person_name, Place) -> rpc(person_db_server, {add, Person_name, Place}).
find(Person_name)       -> rpc(person_db_server, {find, Person_name}).

%% callback routines
init() -> dict:new().
handle({add, Person_name, Place}, Dict) -> {ok, dict:store(Person_name, Place, Dict)};
handle({find, Person_name}, Dict)       -> {dict:find(Person_name, Dict), Dict}.


%1> server_2:start(person_db_server, person_db_2).
%true

%2> person_db_2:add(joe, "at home").
%ok

%3> person_db_2:find(joe).
%{ok,"at home"}
