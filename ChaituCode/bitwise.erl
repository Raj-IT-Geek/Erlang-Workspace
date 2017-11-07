-module(bitwise).
-export([pos2int/1,
         union/2,
         minus/2,
         intersection/2,
         is_intersection/2]).

pos2int(Pos) ->
  1 bsl (Pos -1).

union(A, B) ->
  A bor B.

minus(A, B) ->
  A bxor B.

intersection(A, B) ->
  A band B.

is_intersection(A, B) ->
  A band B > 0.
