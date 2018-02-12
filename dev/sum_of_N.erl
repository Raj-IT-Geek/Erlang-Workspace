-module(sum_of_N).
-export([sum/1, sum_tail/1]).

% Recursive.
sum(Range) when Range =< 0 -> 0;
sum(Range) -> Range+sum(Range-1).


% Tail-Recursive.
sum_tail(Range) -> sum_tail(0, Range).

sum_tail(Sum, 0) -> Sum;
sum_tail(Sum, Range) -> sum_tail(Sum+Range, Range-1).
