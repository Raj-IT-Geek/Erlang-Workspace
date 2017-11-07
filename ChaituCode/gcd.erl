-module(gcd).
-export([oof/2]).

oof(K, M) when K == M -> M;
oof(K, M) when K > M -> oof(K - M, M);
oof(K, M) -> oof(K, M - K).
