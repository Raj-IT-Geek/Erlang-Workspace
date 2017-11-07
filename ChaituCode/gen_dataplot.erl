%%%-------------------------------------------------------------------
%%% @author danny
%%% @copyright (C) 2017, danny
%%% @doc
%%%
%%% @end
%%% Created : 2017-10-19 07:14:39.937288
%%%-------------------------------------------------------------------
-module(gen_dataplot).

-behaviour(gen_server).

%% API
-export([start_link/3,
         start_link/4,
         notify/3,
         notify/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Module, Args, Options) ->
  gen_server:start_link(?MODULE, [Module|Args], Options).

start_link(Reg, Module, Args, Options) ->
  gen_server:start_link(Reg, ?MODULE, [Module|Args], Options).

notify(Server, Event, Entity) ->
  notify(Server, Event, Entity, erlang:localtime()).

notify(Server, Event, Entity, DateTime) ->
  gen_server:cast(Server, {notify, Event, Entity, DateTime}).

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
init([Module|Args]) ->
  case Module:init(Args) of
    {ok, State} ->
      {ok, {Module, State}};
    Error ->
      Error
  end.

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
handle_cast({notify, Event, Entity, DateTime}, {Module, State}) ->
  case catch Module:handle_notify({Event, Entity, DateTime}, State) of
    {ok, Events, NewState} ->
      handle_notify(Events),
      {noreply, {Module, NewState}};
    Error ->
      {stop, Error, State}
  end;
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

handle_notify(Events) ->
  {ok, Events}.
