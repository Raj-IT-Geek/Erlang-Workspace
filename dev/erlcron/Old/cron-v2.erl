-module(cron).
-behaviour(gen_server).

-record(cron, {
                name,
                function,
                schedule
              }).
%% API
-export([start/0,
         stop/0,
         add_job/3,
         view_job/1,
         list_jobs/0,
         modify_job/2,
         delete_job/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(WAIT, 15000).


%%% ---------------------------------- API ---------------------------------

start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

add_job(Name, Fun, Schedule) -> handle_add_job(Name, Fun, Schedule).

list_jobs() -> handle_list_jobs().

view_job(Name) -> handle_view_job(Name).

modify_job(Name, Schedule) -> handle_modify_job(Name, Schedule).

delete_job(Name) -> handle_delete_job(Name).



%%% ------------------------ gen_server callbacks ------------------------

init([]) ->
  mnesia:start(),
  process_flag(trap_exit, true),
  {ok, #{}, ?WAIT}.


handle_call(_Request, _From, SysDateTime) ->
  Reply = ok,
  {reply, Reply, SysDateTime}.


handle_cast(stop, SysDateTime) -> {stop, normal, SysDateTime};
handle_cast(_Msg, SysDateTime) -> {noreply, SysDateTime}.


handle_info(timeout, SysDateTime) ->
    case sys_date_time() of
        SysDateTime ->
            {noreply, SysDateTime, ?WAIT};
        NewSysDateTime ->
            io:format("~n~n-------------Checking Cron Jobs~p~n", [NewSysDateTime]),
            handle_jobs(NewSysDateTime),
            {noreply, NewSysDateTime, ?WAIT}
    end;

handle_info(Info, SysDateTime) ->
  io:format("Received Info ~p", [Info]),
  {noreply, SysDateTime, ?WAIT}.


terminate(_Reason, _SysDateTime) -> ok.

code_change(_OldVsn, SysDateTime, _Extra) -> {ok, SysDateTime}.



%%% ------------------------- Internal Functions -----------------------

sys_date_time() ->
  {Date, {H, M, _S}} = erlang:localtime(),
  {Date, {H, M, 0}}.


%%  cron:add_job(greet, {io, format, ["Hello there!"]}, {'1', '2', '3', '4', '5'}).
%%  cron:add_job(greet1, {io, format, ["Hello there!"]}, {'1', '2', '3', '4', '5'}).
handle_add_job(_Name, _Function, _Schedule) ->
  CronRecord = #cron{name=_Name, function=_Function, schedule=_Schedule},
  F = fun() -> mnesia:write(CronRecord) end,
  mnesia:transaction(F).

%%  cron:view_job(greet).
handle_view_job(_Name) -> mnesia:dirty_read(cron, _Name).

%% cron:list_jobs().
handle_list_jobs() ->
  lists:foreach(
    fun(CronRecord) -> io:format("~n~p", [CronRecord]) end,
    lists:flatten([mnesia:dirty_read(cron, X) || X <- mnesia:dirty_all_keys(cron)])
  ).


%% cron:modify_job(greet1, {'1', '1', '1', '1', '1'}).
handle_modify_job(_Name, _Schedule) ->
  case mnesia:dirty_read(cron, _Name) of
         [] -> {error, entry_not_found};
         [CronRecord] -> mnesia:dirty_write(CronRecord#cron{schedule=_Schedule})
  end.


%%  cron:delete_job(greet1).
handle_delete_job(_Name) ->
  CronRecord = {cron, _Name},
  F = fun() -> mnesia:delete(CronRecord) end,
  mnesia:transaction(F).


handle_jobs(SysDateTime) ->
  io:format("~n~nEntered handle_jobs module~n"),
  lists:foreach(
    fun(CronRecord) -> handle_job(SysDateTime, CronRecord) end,
    lists:flatten([ mnesia:dirty_read(cron, CronRecord) || CronRecord <- mnesia:dirty_all_keys(cron) ])
   ).



handle_job(SysDateTime, #cron{name = _Name,
               						function = {M, F, A},
               						schedule = Schedule}) ->
  io:format("~n~nEntered handle_job module with~n Datetime: ~p~nSchedule: ~p~n", [SysDateTime, Schedule]),
  case is_schedule_now(SysDateTime, Schedule) of
  	false -> io:format("~n~nFALSE~n~n"), ok;
  	true ->
      io:format("~n~n...Executing... ~n~p, ~n~p, ~n~p~n~n", [M,F,A]),
      spawn(M, F, [A])
  end.


is_schedule_now(SysDateTime, JobSchedule) ->
  io:format("~n~nEntered is_schedule_now...~nSysDatetime:~p~nJobSchedule:~p~n", [SysDateTime, JobSchedule]),
  {{_SysYYYY, SysMM, SysDD}, {SysHour, SysMinute, _SysSecond}}  = SysDateTime,
  {JobMinute, JobHour, JobDayOfMonth, JobMonth, JobDayOfWeek} = JobSchedule,
  SysDay = calendar:day_of_the_week({_SysYYYY, SysMM, SysDD}),
  MatchVerb =   [{SysMinute, JobMinute}, {SysHour, JobHour},
                 {SysDD, JobDayOfMonth}, {SysMM, JobMonth},
                 {SysDay, JobDayOfWeek}],
  io:format("~n~nMatch Verb: ~p~n~n", [MatchVerb]),
  lists:all(fun try_match/1, MatchVerb).


try_match({_, '_'}) -> true;
try_match({X, X}) -> true;
try_match({X, [$/|Vs]}) ->
	case catch list_to_integer(Vs) of
		{'EXIT', _Reason} -> false;
		V ->
  			if
  				X rem V == 0 -> true;
  				true -> false
  			end
	end;
try_match({X,L}) when is_list(L) -> lists:member(X, L);
try_match(_) -> io:format("~n~n LAST~n~n"), false.
