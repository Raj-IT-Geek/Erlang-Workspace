-module(double).
-compile(export_all).

oof(X) -> X * 2.

oof_list(L) ->
  oof_list(L, []).

oof_list([], DoubleList) ->
  lists:reverse(DoubleList);
oof_list([H|T], DoubleList) ->
  D = oof(H),
  oof_list(T, [D|DoubleList]).

oof_list2(L) ->
  [X * 2 || X <- L].

fixed_deposit(Amounts) ->
  lists:sum(lists:map(
    fun(Amount) -> 14/100 * Amount + Amount end,
    Amounts
   )).

fixed_deposit_total(Amounts) ->
  lists:foldl(
    fun(Amount, Accumulator) ->
      Interest = Amount * 14/100,
      AmountEnd = Amount + Interest,
      Accumulator + AmountEnd
    end,
    0,
    Amounts
   ).

fixed3(Amounts) ->
  lists:mapfoldl(
    fun(Amount, Acc) ->
        AmountEnd = 14/100 * Amount + Amount,
        {AmountEnd, Acc + AmountEnd}
    end,
    0,
    Amounts
   ).

testwrap() ->
  io:format("Hey testing the wrapping funtionality\
  Ok and go ahead~n").
