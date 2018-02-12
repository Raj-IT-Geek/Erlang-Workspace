-module(even_in_list_recur).
-export([even/1]).

even([]) -> [];
even(List) -> even(List, []).

even([],Even_List) -> lists:reverse(Even_List);
even([Head|Tail], Even_List) when Head rem 2 == 0 -> 
                                              even(Tail, [Head | Even_List]);
even([_Head | Tail], Even_List) -> even(Tail, Even_List).