-module(average_tail_recur).
-export([average/1]).

average([]) -> 0;
average(List) -> average_acc(List, 0, 0).

average_acc([], Sum, Length) -> Sum / Length;
average_acc([H | T], Sum, Length) -> average_acc(T, Sum + H, Length + 1).