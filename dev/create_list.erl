-module(create_list).
-export([create_list/1]).

create_list(Range) -> create_list([], Range).

create_list(List, Range) when Range =< 0 -> lists:reverse(List);
create_list(List, Range) -> create_list(lists:append(List,[Range]), Range-1).  