-module(flatten_nested_lists).
-export([flatten/1]).

% Write a function that, given a list of nested lists, will return a flat list.
% flatten([[1,[2,[3],[]]], [[[4]]], [5,6]]) ⇒ [1,2,3,4,5,6].

flatten(NestedLists) -> lists:reverse(process(NestedLists)).

process(NestedLists) -> process(NestedLists, []).

process([], FinalList) -> FinalList;
process([List1 | T], FinalList) -> process(T, concat([process(List1), FinalList]));
process(X, FinalList) -> [X|FinalList].

concat(Lists) -> concat(Lists, []).

concat([], MergedList) -> lists:reverse(MergedList);
concat([H|T], MergedList) -> concat(T, merge(H, MergedList)).

merge([], List2) -> List2;
merge([H|T], List2) -> merge(T, [H|List2]).