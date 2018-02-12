-module(create_list_range_reverse).
-export([create_list/2]).

create_list(R1, R2) -> create_list(R1, R2, []). 

create_list(R1, R2, List) when R1 > R2 -> List;
create_list(R1, R2, List) -> create_list(R1+1, R2, [R1|List]).  