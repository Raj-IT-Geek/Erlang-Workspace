-module(sierpinski).
-compile(export_all).

main(R, C, N) ->
  lists:foreach(
    fun(A) -> io:format("~s~n", A) end,
    lists:foldl(fun(_X, D) -> fractalize(D) end,
                draw(R, C),
                lists:seq(1, N))
   ).

draw(R, C) ->
  lists:zipwith(
    fun(Canvas, Triangle) ->
        replace(Canvas, Triangle)
    end,
    canvas(R, C),
    triangle(C)
   ).

canvas(R, C) when ((R * 2) - 1) == C ->
  [ [ $_ || _X <- lists:seq(1, C) ] || _Y <- lists:seq(1, R) ].

triangle(H) ->
  [ [ $1 || _X <- lists:seq(1, Y) ] || Y <- lists:seq(1, H, 2)].

replace(L, SL) ->
  string:left(L, gap(L, SL)) ++ SL ++
  string:right(L, gap(L, SL)).

gap(L, SL) ->
  (length(L) - length(SL)) div 2.

print([]) -> ok;
print([H|D]) ->
  io:format("~s~n", [H]),
  print(D).

fractalize(D) ->
  D.

