-module(create_cron_table).
-export([do_this_once/0]).

-record(cron, {name, function, schedule}).

do_this_once() ->
    mnesia:stop().
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(cron, [{attributes, record_info(fields, cron)}]).
