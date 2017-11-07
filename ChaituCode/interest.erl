-module(interest).
-export([formula/1,
         formula1/2]).

formula(F) ->
  fun(X) -> (F / X) * 100 end.

formula1(F, X) ->
  F(X * 66).
