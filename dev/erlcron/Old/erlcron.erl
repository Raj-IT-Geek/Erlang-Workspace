-module(erlcron).
-export([start/0]).
-define(CRONFILE, "crontab").

start() -> readFile(?CRONFILE).

readFile(File) ->
    {ok, CronFile} = file:open(File, [read]),
    processLines(CronFile, 1).

processLines(CronFile, N) ->
    case io:get_line(CronFile, "") of
        eof -> ok;
        Line ->
            processTask(Line),
            processLines(CronFile, N+1)
    end.

processTask(CronLine) ->
    [JobMinute, JobHour, JobDom, JobMonth, JobDow, JobModule, JobFunction | JobArgs] =
                re:split(string:trim(CronLine), " ",[{return,list}]),

    {{SysYear, SysMonth, SysDom}, {SysHour, SysMinute, SysSeconds}} = erlang:localtime(),
    io:format("~nSystem Time... ~p:~p:~p, ~p:~p:~p",
                [SysYear, SysMonth, SysDom, SysHour, SysMinute, SysSeconds]),

    CronTime = {{SysYear, JobMonth, JobDom}, {JobHour, JobMinute}},
    io:format("~nCron: ~p~n", [CronTime]),

    M = format({module, JobModule}),
    F = format({function, JobFunction}),
    A = format({arguments, JobArgs}),
    erlang:apply(M, F, [A]).


% ----------- formatting -------------
format({module, M}) -> list_to_atom(M);
format({function, F}) -> list_to_atom(F);
format({arguments, A}) -> format(arguments, A, []).

format(arguments, [], List) -> lists:reverse(List);
format(arguments, [H|T], List) -> format(arguments, T, [list_to_atom(H)|List]).
