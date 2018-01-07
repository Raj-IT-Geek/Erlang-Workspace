-module(applySeq).
-compile(export_all).

do(X, Seq) ->
  lists:foldl(
    fun(F, A) ->
      F(A)
    end,
    X,
    Seq
   ).
