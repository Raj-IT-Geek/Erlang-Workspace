-module(test).

-compile(export_all).

-define(INTERVALS, 96).

interval_seq({StartDate, SInt}, {EndDate, EInt}) ->
  [ [ {Date, Int} || Int <- lists:seq(SInt1, EInt1) ]
    || {Date, SInt1, EInt1} <-
       [{StartDate, SInt, ?INTERVALS}|[{D, 1, ?INTERVALS}
                                       || D <- date_seq(
                                                 next_date(StartDate),
                                                 prev_date(EndDate))]]
   ++ [{EndDate, 1, EInt}]].

date_seq(StartDate, EndDate) ->
  [ calendar:gregorian_days_to_date(Day) ||
    Day <- lists:seq( calendar:date_to_gregorian_days(StartDate),
                      calendar:date_to_gregorian_days(EndDate) ) ].

next_date(Date) ->
  calendar:gregorian_days_to_date(
    calendar:date_to_gregorian_days(Date) + 1
   ).

prev_date(Date) ->
  calendar:gregorian_days_to_date(
    calendar:date_to_gregorian_days(Date) - 1
   ).

