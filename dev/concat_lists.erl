-module(concat_lists).
-export([concat/1, merge/2]).

% Write a function that, given a list of lists, will concatenate them.
% concatenate([[1,2,3], [], [4, five]]) ⇒ [1,2,3,4,five].

concat(Lists) -> concat(Lists, []).

concat([], MergedList) -> lists:reverse(MergedList);
concat([H|T], MergedList) -> concat(T, merge(H, MergedList)).

merge([], List2) -> List2;
merge([H|T], List2) -> merge(T, [H|List2]).