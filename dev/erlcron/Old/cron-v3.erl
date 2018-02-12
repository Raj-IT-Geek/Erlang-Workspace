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
-define(WAIT, 10000).


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
            %handle_jobs(SysDateTime),
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
  CronJob = #cron{name=_Name, function=_Function, schedule=_Schedule},
  F = fun() -> mnesia:write(CronJob) end,
  mnesia:transaction(F).

%%  cron:view_job(greet).
handle_view_job(_Name) -> mnesia:dirty_read(cron, _Name).

%% cron:list_jobs().
handle_list_jobs() ->
  lists:foreach(
    fun(CronJob) -> io:format("~n~p", [CronJob]) end,
    lists:flatten([mnesia:dirty_read(cron, X) || X <- mnesia:dirty_all_keys(cron)])
  ).


%% cron:modify_job(greet1, {'1', '1', '1', '1', '1'}).
handle_modify_job(_Name, _Schedule) ->
  case mnesia:dirty_read(cron, _Name) of
         [] -> {error, entry_not_found};
         [CronJob] -> mnesia:dirty_write(CronJob#cron{schedule=_Schedule})
  end.


%%  cron:delete_job(greet1).
handle_delete_job(_Name) ->
  CronJob = {cron, _Name},
  F = fun() -> mnesia:delete(CronJob) end,
  mnesia:transaction(F).


handle_jobs(SysDateTime) ->
  lists:foreach(
    fun(CronJob) -> handle_job(SysDateTime, CronJob) end,
    lists:flatten([ mnesia:dirty_read(cron, CronJob) || CronRecord <- mnesia:dirty_all_keys(cron) ])
   ).



handle_job(SysDateTime, #cron{name = _Name,
               						function = {M, F, A},
               						schedule = Schedule}) ->
  case match_times(SysDateTime, Schedule) of
  	false -> ok;
  	true -> io:format("~n~nTRUE~n~n"), spawn(M, F, [A])
  end.


match_times(SysDateTime, JobSchedule) ->
  {{_SysYYYY, SysMM, SysDD}, {SysHour, SysMinute, _SysSecond}}  = SysDateTime,
  {JobMinute, JobHour, JobDayOfMonth, JobMonth, JobDayOfWeek} = JobSchedule,
  MatchVerb =   [{SysMinute, JobMinute}, {SysHour, JobHour},
                 {SysDD, JobDayOfMonth}, {SysMM, JobMonth},
                 {calendar:day_of_the_week({_SysYYYY, SysMM, SysDD}), JobDayOfWeek}],
  io:format("~nMatch Verb: ~p~n", [MatchVerb]),
  lists:all(fun match_values/1, MatchVerb).

match_values({_X, '_'}) -> io:format("~n~nMatched _, '_'; ~p~n", [_X]), true;
match_values({Val, Val}) -> io:format("~n~nMatched Val: ~p, ~p~n", [Val,Val]), true;
match_values({Val, [$/|Interval]}) ->
    io:format("~n~nMatched ~p/~p~n", [Val, Interval]),
    case Val rem list_to_integer(Interval) of
      0 -> true;
      _ -> false
		end;
match_values({Val, ValuesList}) when is_list(ValuesList) ->
      io:format("~n~nMatched Val, [Val1, Val2...]~n"),
      lists:member(Val, ValuesList);
match_values(_Y) -> io:format("~n~nMatched _;  ~p~n", [_Y]), false.
