-module(addrbook).
-export([init/0,
        add/3,
        view/1,
        list/0,
        remove/1]).

-record(addrbook, {
         name,
         phone,
         city
        }).

init() ->
    mnesia:create_table ( addrbook, [ {attributes, record_info(fields, addrbook)}, {ram_copies, [node()] } ] ).

add(Name, Phone, City) ->
    mnesia:dirty_write(#addrbook{name = Name, phone = Phone, city = City}).

view(Name) ->
    mnesia:dirty_read(addrbook, Name).

list() ->
    [ mnesia:dirty_read(addrbook, Key) || Key <- mnesia:dirty_all_keys(addrbook) ].

remove(Name) ->
    mnesia:dirty_delete(addrbook, Name).
