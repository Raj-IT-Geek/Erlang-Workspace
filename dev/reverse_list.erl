-module(reverse_list).
-export([reverse/1, reverse/2]).

% Write a function that, given a list, will reverse the order of the elements.
% reverse([1,2,3]) ⇒ [3,2,1].

reverse(List) -> reverse(List, []).

reverse([], ReversedList) -> ReversedList;
reverse([H|T], ReversedList) -> reverse(T, [H|ReversedList]).
