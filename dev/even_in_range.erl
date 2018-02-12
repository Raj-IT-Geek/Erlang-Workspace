-module(even_in_range).
-export([even/1]).

even(Range) -> even(Range, []).

even(Range, List) when Range =< 0 -> List;
even(Range, List) when Range rem 2 == 0 -> even(Range-1, [Range | List]);
even(Range, List) -> even(Range-1, List). 