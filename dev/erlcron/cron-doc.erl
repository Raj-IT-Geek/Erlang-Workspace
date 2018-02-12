
-module(cron).

-behaviour(gen_server).

-record(cron, {
          name,
          function,
          schedule
         }).
%% API
-export([start_link/0,
         view_job/1,
         list_jobs/0,
         add_job/3,
         modify_job/2,
         delete_job/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(WAIT, 30000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% List All jobs
%%
%% @spec list_jobs() -> {ok, JobNames} | {error, Error}
%% JobNames :: list of job names
%% @end
%%--------------------------------------------------------------------
list_jobs() ->
  handle_list_jobs().


%%--------------------------------------------------------------------
%% @doc
%% Add a job for scheduling
%%
%% @spec add_job(Name, Fun, Schedule) -> ok | {error, Error}
%% Schedule ==>> { Min, Hour, DayOfMonth, Month, DayOfWeek }
%% Min ==>> '_' | "/x" | [x,x1,x2,x3] | x :: x => {1, 60}
%% Hour ==>> '_' | "/x" | [x,x1,x2,x3] | x :: x => {0, 23}
%% DayOfMonth ==>> '_' | "/x" | [x,x1,x2,x3] | x :: x => {1, 31}
%% Month ==>> '_' | "/x" | [x,x1,x2,x3] | x :: x => {1, 12}
%% DayOfWeek ==>> '_' | [x, x1, x2] x => {0, 7} where sunday is 0 and 7
%% @end
%%--------------------------------------------------------------------
add_job(Name, Fun, Schedule) ->
  handle_add_job(Name, Fun, Schedule).


%%--------------------------------------------------------------------
%% @doc
%% View a job
%%
%% @spec view_job(Name) -> {ok, JobDetails} | {error, Error}
%% JobDetails :: Record of the job
%% @end
%%--------------------------------------------------------------------
view_job(Name) ->
  handle_view_job(Name).


%%--------------------------------------------------------------------
%% @doc
%% Modify a job for scheduling
%%
%% @spec modify_job(Name, Schedule) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
modify_job(Name, Schedule) ->
  handle_modify_job(Name, Schedule).


%%--------------------------------------------------------------------
%% @doc
%% delete a job
%%
%% @spec delete_job(Name) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
delete_job(Name) ->
  handle_delete_job(Name).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  {ok, #{}, ?WAIT}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State, ?WAIT}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State, ?WAIT}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
  case localtime_minute() of
    State ->
      {noreply, State, ?WAIT};
    NewState ->
      handle_jobs(NewState),
      {noreply, NewState, ?WAIT}
  end;
handle_info(Info, State) ->
  lager:info("Received Info ~p", [Info]),
  {noreply, State, ?WAIT}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

localtime_minute() ->
  {Date, {H, M, _S}} = erlang:localtime(),
  {Date, {H, M, 0}}.


%%--------------------------------------------------------------------
%% Implement the function to do the following work
%%    1. First check if a job with the Name already exists, if exists
%%      return {error, already_exists} or else proceed
%%    2. Insert into mnesia using dirty_write - name, fun and schedule
%%    3. return ok
%%--------------------------------------------------------------------
handle_add_job(_Name, _Fun, _Schedule) ->
  todo.


%%--------------------------------------------------------------------
%% Implement the function to do the following work
%%    1. Read the mnesia table using dirty_read to get the record for
%%       job with name Name
%%    2. If the return value is [], then return {error, "Cron job not found"}
%%    3. Capture the return value and replace the schedule in the
%%       record
%%    4. Write back the changed record into mnesia using dirty_write
%%    5. return ok
%%--------------------------------------------------------------------
handle_modify_job(_Name, _Schedule) ->
  todo.


%%--------------------------------------------------------------------
%% Implement the function to do the following work
%%    1. Read the mnesia table using dirty_read to get the record for
%%       job with name Name
%%    2. If the return value is [], then return {error, "Cron job not found"}
%%    3. Delete the record from mnesia using dirty_delete
%%    4. return ok
%%--------------------------------------------------------------------
handle_delete_job(_Name) ->
  todo.


%%--------------------------------------------------------------------
%% Implement the function to do the following work
%%    1. Get all keys from mnesia using dirty_all_keys
%%    2. Return {ok, ListOfMnesiaKeys}
%%    3. Any other response, return {error, Error}
%%--------------------------------------------------------------------
handle_list_jobs() ->
  todo.


%%--------------------------------------------------------------------
%% Implement the function to do the following work
%%    1. Read the mnesia table using dirty_read to get the record for
%%       job with name Name
%%    2. If the return value is [], then return {error,"Cron job not found"}
%%    3. If else Return {ok, JobRecord}
%%--------------------------------------------------------------------
handle_view_job(_Name) ->
  todo.


%%--------------------------------------------------------------------
%% Implemet the function to do the following work
%%    1. Pick up all the jobs from mnesia using dirty_all_keys and
%%       dirty_read
%%    2. Check all the jobs that needs to be run this minute
%%    3. Execute the jobs
%%--------------------------------------------------------------------
handle_jobs(DateTime) ->
  lists:foreach(
    fun(Job) ->
        handle_job(DateTime, Job)
    end,
    lists:flatten([ mnesia:dirty_read(cron, X) || X <- mnesia:dirty_all_keys(cron) ])
   ).


%%--------------------------------------------------------------------
%% Implemet the function to do the following work
%%    1. Extract schedule and check if the current time matches the schedule
%%    2. If the time matches just print the function name
%%    3. Get the current date time using the function erlang:localtime()
%%    4. Now you have to match the Month, Date, DayOfWeek, Hour and Minute
%%       as per the spec
%%    5. to get day of the week from date call the function
%%       calendar:day_of_the_week({YYYY, MM, DD})
%%    6. To pattern match the spec you have to follow below rules
%%        a. '_' means matchs anything
%%        b. 4 means matches digit
%%        c. [2,3,4] means matches the values 2, 3, or 4
%%        d. "/15" means matches any value that is divisible by 15 using
%%           45 rem 15 == 0
%%--------------------------------------------------------------------
handle_job(_DateTime, #cron{name = _Name,
                           function = {_M, _F, _A},
                           schedule = _Schedule}) ->
  todo.
