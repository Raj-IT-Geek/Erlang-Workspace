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


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(timeout, ThenDateTime) ->
    case sys_date_time() of
        ThenDateTime ->
            %handle_jobs(SysDateTime),
            {noreply, ThenDateTime, ?WAIT};
        NowDateTime ->
            %io:format("~n~n-------------Checking Cron Jobs~p~n", [NowDateTime]),
            handle_jobs(NowDateTime),
            {noreply, NowDateTime, ?WAIT}
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


handle_add_job(_Name, _Function, _Schedule) ->
  CronJob = #cron{name=_Name, function=_Function, schedule=_Schedule},
  F = fun() -> mnesia:write(CronJob) end,
  mnesia:transaction(F).

handle_view_job(_Name) -> mnesia:dirty_read(cron, _Name).

handle_list_jobs() ->
  lists:foreach(
    fun(CronJob) -> io:format("~n~p", [CronJob]) end,
    lists:flatten([mnesia:dirty_read(cron, X) || X <- mnesia:dirty_all_keys(cron)])
  ).


handle_modify_job(_Name, _Schedule) ->
  case mnesia:dirty_read(cron, _Name) of
         [] -> {error, entry_not_found};
         [CronJob] -> mnesia:dirty_write(CronJob#cron{schedule=_Schedule})
  end.


handle_delete_job(_Name) ->
  CronJob = {cron, _Name},
  F = fun() -> mnesia:delete(CronJob) end,
  mnesia:transaction(F).


handle_jobs(SysDateTime) ->
  lists:foreach(
    fun(CronJob) ->
        handle_job(SysDateTime, CronJob)
    end,
    lists:flatten([ mnesia:dirty_read(cron, CronJob) || CronJob <- mnesia:dirty_all_keys(cron) ])
    ).


handle_job(Now, #cron{name = _Name,
                            function = {M, F, A},
                            schedule = Schedule}) ->
  case match_times(Now, Schedule) of
  	false -> ok;
  	true -> spawn(M, F, [A])
  end.


match_times(Now, Schedule) ->
  {{_SYYYY, SMM, SDD}, {SHour, SMinute, _SSecond}}  = Now,
  {JMinute, JHour, JDayOfMonth, JMonth, JDayOfWeek} = Schedule,
  MatchList =   [{SMinute, JMinute}, {SHour, JHour},
                 {SDD, JDayOfMonth}, {SMM, JMonth},
                 {calendar:day_of_the_week({_SYYYY, SMM, SDD}), JDayOfWeek}],
  lists:all(fun match_values/1, MatchList).

match_values({_Value, '_'}) -> true;
match_values({Value, Value}) -> true;
match_values({Value, [$/|Interval]}) ->
    case catch (Value rem list_to_integer(Interval)) of
      {'EXIT', _Reason} -> false;
      0 -> true;
      _ -> false
		end;
match_values({Value, ValuesList}) when is_list(ValuesList) -> lists:member(Value, ValuesList);
match_values(_Y) -> false.
