-module(intervals).

-compile(export_all).

-define(DINT, 1440).

days_from_period(SDate, EDate, Inc) ->
  [ calendar:gregorian_days_to_date(Day)
    || Day <- lists:seq(calendar:date_to_gregorian_days(SDate),
                        calendar:date_to_gregorian_days(EDate),
                        Inc) ].

next_date(Date, N) ->
  calendar:gregorian_days_to_date(
    calendar:date_to_gregorian_days(Date) + N
   ).

list(SDate, EDate, Int) when Int > ?DINT,
                             (Int rem ?DINT) == 0 ->
  Inc = Int div ?DINT,
  [
   {{Date, 1}, {next_date(Date, Inc - 1), ?DINT}} ||
   Date <- days_from_period(SDate, EDate, Inc)
  ];
list(SDate, EDate, Int) when (?DINT rem Int) == 0 ->
  [
   {{Date, S}, {Date, E}} ||
   Date <- days_from_period(SDate, EDate, 1),
   {S, E} <- [{X, X + Int - 1} || X <- lists:seq(1, ?DINT, Int) ]
  ].

list1([], List) ->
  lists:reverse(List);
list1([Date, NextDate|DateList], List) ->
  list1([NextDate|DateList],
        [{{Date, 1}, {NextDate, ?DINT}}|List]).

