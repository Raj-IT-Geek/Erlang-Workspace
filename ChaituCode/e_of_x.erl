-module(e_of_x).
-compile(export_all).

eofx(N, T) ->
  eofx(N, 1, 1, 1, T, 1).

eofx(_N, _E, _F, T, T, S) ->
  S;
eofx(N, E, F, I, T, S) ->
  E1 = E * N,
  F1 = F * I,
  eofx(N, E1, F1, I + 1, T, S + E1/F1).

