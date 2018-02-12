-module(sum_of_range).
-export([sum/2]).

sum(R1, R2) -> sum(R1, R2, 0).

sum(R1, R2, Sum) when R2 < R1 -> Sum;
sum(R1, R2, Sum) -> sum(R1, R2-1, Sum+R2).