-module(filter_list).
-export([filter/2, scan/3]).

% Write a function that, given a list of integers and an integer, will return all integers
%  smaller than or equal to that integer.
% filter([1,2,3,4,5], 3) ⇒ [1,2,3].

filter(List, Number) -> scan(List, Number, []).

scan([], _Number, FilteredList) -> lists:reverse(FilteredList);
scan([H|T], Number, FilteredList) when H =< Number -> scan(T, Number, [H|FilteredList]);
scan([_H|T], Number, FilteredList) -> scan(T, Number, FilteredList).