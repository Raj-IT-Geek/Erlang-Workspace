-module(matrix).
-export([transpose/1,
         multiply/2]).

transpose([H|_] = Matrix) ->
  lists:reverse(
    lists:foldl(
      fun(I, M) ->
          [ [lists:nth(I, X) || X <- Matrix] | M ]
      end,
      [],
      lists:seq(1, length(H))
     )
   ).

multiply(A, B) when length(hd(A)) == length(B) ->
  [[lists:sum([lists:nth(I, A1) * lists:nth(I, B1)|| I<-lists:seq(1, length(A1))])|| B1 <- lists:foldr(fun(J, M) -> [[lists:nth(J, X) || X <- B] | M] end,[],lists:seq(1, length(hd(B))))]|| A1 <- A];
multiply(_, _) ->
  not_possible.

