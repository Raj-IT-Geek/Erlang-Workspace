%%%-------------------------------------------------------------------
%%% @author danny
%%% @copyright (C) 2017, danny
%%% @doc
%%%
%%% @end
%%% Created : 2017-01-12 15:39:55.834229
%%%-------------------------------------------------------------------
-module(dataplot_acd).

-behaviour(gen_server).

-record(call_events, {
          call_id,
          acd,
          queue,
          agent,
          status,
          datetime
         }).

%% API
-export([start_link/0,
         notify/3,
         notify/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

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
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

notify(CallId, Entity, Event) ->
  notify(CallId, Entity, Event, erlang:localtime()).

notify(CallId, Entity, Event, DateTime) ->
  gen_server:cast(?SERVER, {notify, CallId, Entity, Event, DateTime}).

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
  mnesia:start(),
  mnesia:create_table(call_events,
                      [{attributes, record_info(fields, call_events)}]),
  {ok, #{}}.

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
  {reply, Reply, State}.

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
handle_cast({notify, CallId, Entity, Event, DateTime}, State) ->
  io:format("cast is ~p~n",
            [{CallId, Entity, Event, DateTime}]),
  handle_notify(CallId, Entity, Event, DateTime),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

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
handle_info(_Info, State) ->
  {noreply, State}.

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

handle_notify(CallId, Entity, Event, DateTime) ->
  [ dataplot:notify(EntityA, EventA, DateTimeA) ||
    {EntityA, EventA, DateTimeA} <- get_events(CallId,
                                               Entity,
                                               Event,
                                               DateTime) ].

get_events(CallId,
           {queue, QueueId},
           incoming_call,
           DateTime) ->
  case mnesia:dirty_read(call_events, CallId) of
    [] ->
      mnesia:dirty_write(#call_events{call_id = CallId,
                                      acd = QueueId,
                                      queue = QueueId,
                                      status = incoming_call,
                                      datetime = DateTime}),
      [  {QueueId, "acd_call_arrive", DateTime},
         {QueueId, "queue_call_arrive", DateTime},
         {QueueId, "acd_wait_start", DateTime},
         {QueueId, "peak_wait_up", DateTime},
         {QueueId, "queue_wait_start", DateTime}   ];
    [CallEvent] ->
      mnesia:dirty_write(CallEvent#call_events{queue = QueueId}),
      [  {QueueId, "queue_call_arrive", DateTime},
         {QueueId, "peak_wait_up", DateTime},
         {QueueId, "queue_wait_start", DateTime}   ]
  end;
get_events(CallId,
           {agent_queue, AgentId, QueueId},
           agent_answered,
           DateTime) ->
  case mnesia:dirty_read(call_events, CallId) of
    [] ->
      [];
    [#call_events{acd = Acd, queue = QueueId} = CallEvent] ->
      mnesia:dirty_write(CallEvent#call_events{agent = AgentId,
                                                status = agent_answered}),
      [ {Acd, "acd_call_answer", DateTime},
        {Acd, "acd_wait_stop", DateTime},
        {Acd, "acd_talk_start", DateTime},
        {QueueId, "peak_wait_down", DateTime},
        {QueueId, "peak_talk_up", DateTime},
        {QueueId, "queue_wait_stop", DateTime},
        {{AgentId, QueueId}, "aq_answer", DateTime},
        {{AgentId, QueueId}, "aq_talk_start", DateTime} ];
    [#call_events{acd = Acd, queue = OtherQueueId} = CallEvent] ->
      mnesia:dirty_write(CallEvent#call_events{queue = QueueId,
                                               agent = AgentId,
                                               status = agent_answered}),
      [ {Acd, "acd_call_answer", DateTime},
        {Acd, "acd_wait_stop", DateTime},
        {Acd, "acd_talk_start", DateTime},
        {OtherQueueId, "peak_wait_down", DateTime},
        {QueueId, "peak_talk_up", DateTime},
        {OtherQueueId, "queue_wait_stop", DateTime},
        {{AgentId, QueueId}, "aq_answer", DateTime},
        {{AgentId, QueueId}, "aq_talk_start", DateTime} ]
  end;
get_events(CallId,
           undefined,
           bye,
           DateTime) ->
  case mnesia:dirty_read(call_events, CallId) of
    [] ->
      [];
    [#call_events{acd = Acd,
                  queue = QueueId,
                  status = incoming_call,
                  datetime = StartDateTime}] ->
      mnesia:dirty_delete(call_events, CallId),
      [ {Acd, "acd_call_end", DateTime},
        {Acd, "acd_wait_stop", DateTime},
        {Acd, "acd_wait_abandon_start", StartDateTime},
        {Acd, "acd_wait_abandon_stop", DateTime},
        {QueueId, "queue_call_abandon", DateTime},
        {QueueId, "peak_wait_down", DateTime},
        {QueueId, "queue_wait_stop", DateTime},
        {QueueId, "queue_wait_exit_abandon_start", StartDateTime},
        {QueueId, "queue_wait_exit_abandon_stop", DateTime} ];
    [#call_events{acd = Acd, queue = QueueId, agent = AgentId}] ->
      mnesia:dirty_delete(call_events, CallId),
      [ {Acd, "acd_call_end", DateTime},
        {Acd, "acd_talk_stop", DateTime},
        {QueueId, "peak_talk_down", DateTime},
        {{AgentId, QueueId}, "aq_bye", DateTime},
        {{AgentId, QueueId}, "aq_wrapup", DateTime},
        {{AgentId, QueueId}, "aq_talk_stop", DateTime} ]
  end.
