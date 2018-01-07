-module(pascal).
-export([triangle/1]).

triangle(N) ->
  lists:foreach(
    fun(N1) ->
        lists:foreach(
          fun(R) ->
              io:format("~p ", [pascal(N1 - 1, R - 1)])
          end,
          lists:seq(1, N1)
         ),
        io:format("~n")
    end,
    lists:seq(1, N)
   ).

pascal(N, R) ->
  round(factorial(N) / (factorial(R) * factorial(N - R))).


factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
