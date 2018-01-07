-module(fibonacci).
-export([fib/1, tail_fib/1]).


% ---------------------------------- Recursive fibonacci ----------------------------
fib(0) -> 0;
fib(1) -> 1;

fib(N) when N>0 ->
    io:format("\nN=~w,\n", [N]),
    fib(N-1) + fib(N-2).



% ------------------------------- Tail Recursive fibonacci ----------------------------
tail_fib(N) -> tail_fib_1(N, 0, 1).

tail_fib_1(0, Result, _Next) -> Result;

tail_fib_1(N, Result, Next) ->
    io:format("\nN=~w, Result=~w, Next=~w\n", [N, Result, Next]),
    tail_fib_1(N-1, Next, Result+Next).
