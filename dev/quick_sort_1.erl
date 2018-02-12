-module(quick_sort_1).
-export([qsort/1]).
-import(flatten_nested_lists, [flatten/1]).

% Quicksort
% The head of the list is taken as the pivot; the list is then split according to those
% elements smaller than the pivot and the rest. These two lists are then recursively
% sorted by quicksort, and joined together, with the pivot between them.

qsort([Pivot|[]]) -> Pivot;
qsort([Pivot|List]) -> sort(List, Pivot, [], []).
  
sort([], Pivot, [], List2) -> [Pivot | qsort(List2)];
sort([], Pivot, List1, []) -> [qsort(List1) | Pivot];
sort([], Pivot, List1, List2) -> [qsort(List1), Pivot | qsort(List2)];
sort([H|T], Pivot, List1, List2) when H < Pivot -> sort(T, Pivot, [H|List1], List2);
sort([H|T], Pivot, List1, List2) -> sort(T, Pivot, List1, [H|List2]).

