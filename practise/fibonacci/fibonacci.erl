-module(fibonacci).
-export([fibo/1]).

fibo(N) -> fibo(N, 0, 1).

fibo(0, Result, _Next) -> Result;

fibo(N, Result, Next) -> fibo(N-1, Next, Result+Next).


