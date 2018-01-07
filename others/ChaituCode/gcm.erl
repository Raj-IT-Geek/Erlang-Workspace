-module(gcm).
-export([oof/2]).

oof(A, B) -> round((A/gcd(A,B))*B).

gcd(K, M) when K == M -> M;
gcd(K, M) when K > M -> gcd(K - M, M);
gcd(K, M) -> gcd(K, M - K).
