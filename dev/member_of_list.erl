-module(member_of_list).
-export([member/2]).

member(_, []) -> false;
member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H,T).