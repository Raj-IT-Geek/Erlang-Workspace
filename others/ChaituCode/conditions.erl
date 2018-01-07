-module(conditions).
-compile(export_all).

greater(A, B) ->
  if
    A > B -> A;
    true -> B
  end.

can_marry(Gender, Age) ->
  case {Gender, Age} of
    {male, Age} when Age >= 21 ->
      yes;
    {female, Age} when Age >= 18 ->
      yes;
    _ ->
      no
  end.

member(X, List) ->
  case lists:member(X, List) of
    true ->
      yes;
    false ->
      no
  end.
