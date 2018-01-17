-module(person_db_3).
-export([init/0, add/2, find/1, handle/2]).
-import(server_3, [rpc/2]).

%% client routines
add(Person_name, Place) -> rpc(person_db_server, {add, Person_name, Place}).
find(Person_name)       -> rpc(person_db_server, {find, Person_name}).

%% callback routines
init() -> dict:new().
handle({add, Person_name, Place}, Dict) -> {ok, dict:store(Person_name, Place, Dict)};
handle({find, Person_name}, Dict)       -> {dict:find(Person_name, Dict), Dict}.


%1> server_3:start(person_db_server, person_db_3).
%true

%2> person_db_3:add(joe, "at home").
%ok

%3> person_db_3:find(joe).
%{ok,"at home"}

%4> person_db_3:add(kim, "at off").
%ok

%5> c(person_db_3_new).
%{ok,person_db_3_new}

%6> server_3:swap_code(person_db_server, person_db_3_new).
%ack

%7> person_db_3_new:find(joe).
%{ok,"at home"}

%8> person_db_3_new:all_names().
%[joe,kim]

%9> person_db_3_new:delete(joe).
%ok

%10> person_db_3_new:all_names().
%[kim]
