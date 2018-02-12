-module(create_list_range).
-export([create_list/2]).

create_list(R1, R2) -> create_list(R1, R2, []). 

create_list(R1, R2, List) when R1 > R2 -> lists:reverse(List);
create_list(R1, R2, List) -> create_list(R1+1, R2, [R1|List]). 

% bad logic below - takes atleast 1200% more time bcz of using a library function.
%create_list(R1, R2, List) when R1 > R2 -> List;
%create_list(R1, R2, List) -> create_list(R1+1, R2, lists:append(List, [R1])). 