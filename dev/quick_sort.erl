-module(quick_sort).
-export([qsort/1]).

% Quicksort
% The head of the list is taken as the pivot; the list is then split according to those
% elements smaller than the pivot and the rest. These two lists are then recursively
% sorted by quicksort, and joined together, with the pivot between them.

qsort([]) -> [];
qsort([Pivot|List]) ->
  {Smaller, Larger} = seperate(Pivot,List,[],[]),
  qsort(Smaller) ++ [Pivot] ++ qsort(Larger).

seperate(_,[], Smaller, Larger) -> {Smaller, Larger};
seperate(Pivot, [H|T], Smaller, Larger) when H =< Pivot -> seperate(Pivot, T, [H|Smaller], Larger);
seperate(Pivot, [H|T], Smaller, Larger) -> seperate(Pivot, T, Smaller, [H|Larger]).

