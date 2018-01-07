-module(area).
-export([calc/1,
         double/1,
         multiply/2]).

calc({circle, R}) ->
  Rs = R * R,
  Rs * 3.1415;
calc({rect, S, B}) -> S * B;
calc({square, S}) -> S * S;
calc(_) -> invalid.

double(X) -> X * 2.

multiply(X, Y) -> X * Y.
