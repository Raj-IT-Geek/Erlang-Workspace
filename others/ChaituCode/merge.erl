-module(merge).
-compile(export_all).

lists(Fun, List) ->
  lists(Fun, List, []).

lists(_Fun, [], Merge) ->
  lists:reverse(Merge);
lists(Fun, [H|T], Merge) ->
  {H1, T1} = lists:foldl(
               fun(X, {Ha, Ta}) ->
                   case apply(Fun, [X, Ha]) of
                     false ->
                       {Ha, Ta ++ [X]};
                     {true, Hb} ->
                       {Hb, Ta}
                   end
               end,
               {H, []},
               T
              ),
  lists(Fun, T1, [H1|Merge]).
