-module(db).
-export([new/0, destroy/1, read/2, write/3, match/2, match/3, delete/2, delete/3]).

new() -> [].

write(Person, Location, DB) -> [{Person, Location} | DB].

read(_Person,[]) -> {not_found};
read(Person, [{P,L}|_T]) when Person == P -> {ok,L};
read(Person, [{_P,_L}|T]) -> read(Person, T).

match(Location, DB) -> match(Location, [], DB).

match(_Location, PersonList, []) -> PersonList;
match(Location, PersonList, [{P,L}|T]) when Location == L -> match(Location, [P|PersonList], T);
match(Location, PersonList, [{_P,_L}|T]) -> match(Location, PersonList, T).

delete(Person, DB) -> delete(Person, [], DB).

delete(_Person, UpdatedList, []) -> UpdatedList;
delete(Person, UpdatedList, [{P,_L}|T]) when Person == P -> delete(Person, UpdatedList, T);
delete(Person, UpdatedList, [{P,L}|T]) -> delete(Person, [{P,L}|UpdatedList], T).

destroy([]) -> [];
destroy([_|Tail]) -> destroy(Tail).
