-module(erlcron).
-export([start/0]).
-define(CRONFILE, "crontab").

start() -> readFile(?CRONFILE).

readFile(File) ->
  {ok, CronFile} = file:open(File, [read]),
  fetchLines(CronFile, 1).

fetchLines(CronFile, N) ->
  case io:get_line(CronFile, "") of
      eof -> ok;
      Line -> 
        processTask(Line),
        fetchLines(CronFile, N+1)
  end.

processTask(CronLine) ->
  [Minute, Hour, Dom, Month, Dow, M, F|A] = re:split(string:trim(CronLine), " ",[{return,list}]),

  %io:format("~nCronLine: ~p~n", [CronLine]),
  %[Minute, Hour, Dom, Month, Dow, M, F|A] = re:split(CronLine, " ",[{return,list}]),
  
  JobMinute = form({minute, Minute}),
  JobHour = form({hour, Hour}),
  JobDom = form({dom, Dom}),
  JobMonth = form({month, Month}),
  JobDow = form({dow, Dow}),
  JobM = form({module, M}),
  JobF = form({function, F}),
  JobA = form(arguments, A, []),
  %io:format("~nMinute=~p~nHour=~p~nDom=~p~nMonth=~p~nDow=~p~nM=~p~nF=~p~nA=~p~n", [Minute1, Hour1, Dom1, Month1, Dow1, M1, F1, A1]),
  
  CurrentTime = erlang:localtime(),
  %io:format("~n~p~n", [CurrentTime]),

  SysTime = {{SysYear, SysMonth, SysDom}, {SysHour, SysMinute, SysSeconds}} = CurrentTime,
  io:format("~n~nSystem Time... ~p:~p:~p, ~p:~p:~p~n", [SysYear, SysMonth, SysDom, SysHour, SysMinute, SysSeconds]),
  io:format("~nSys: ~p", [SysTime]),

  CronTime = {{SysYear, JobMonth, JobDom}, {JobHour, JobMinute}},
  io:format("~nCron: ~p~n", [CronTime]),

  %erlang:apply(M1, F1, [A1]).
  ok.


% -----------------------------------------------------------


  form({minute, "*"}) ->  0;
  form({minute, Minute}) -> list_to_integer(Minute);

  form({hour, "*"}) ->  0;
  form({hour, Hour}) -> list_to_integer(Hour);

  form({dom, "*"}) ->  0;
  form({dom, Dom}) -> list_to_integer(Dom);

  form({month, "*"}) ->  0;
  form({month, Month}) -> list_to_integer(Month);

  form({dow, "*"}) ->  0;
  form({dow, Dow}) -> list_to_integer(Dow);

  form({module, M}) -> list_to_atom(M);
  form({function, F}) -> list_to_atom(F).

  form(arguments, [], List) -> lists:reverse(List);
  form(arguments, [H|T], List) -> form(arguments, T, [list_to_atom(H)|List]).
    