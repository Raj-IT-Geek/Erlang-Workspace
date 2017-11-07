%%%-------------------------------------------------------------------
%%% @author danny
%%% @copyright (C) 2016, danny
%%% @doc
%%%
%%% @end
%%% Created : 2016-07-10 13:22:27.931375
%%%-------------------------------------------------------------------
-module(mnesiaOps).

-behaviour(gen_server).

-record(employee, {
          id,
          name,
          dep,
          sal
         }).

%% API
-export([start_link/0,
         add/4,
         read/1,
         update_sal/2]).

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

add(Id, Name, Dep, Sal) ->
  gen_server:call(?SERVER, {add, Id, Name, Dep, Sal}).

read(Id) ->
  gen_server:call(?SERVER, {read, Id}).

update_sal(Id, Sal) ->
  gen_server:call(?SERVER, {update_sal, Id, Sal}).

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
  mnesia:create_table(employee, [{attributes,
                                  record_info(fields, employee)}]),
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
handle_call({add, Id, Name, Dep, Sal}, _From, State) ->
  %Reply = mnesia:dirty_write(#employee{id=Id,
  %                                     name=Name,
  %                                     dep=Dep,
  %                                     sal=Sal}),
  Reply = handle_add(Id, Name, Dep, Sal),
  {reply, Reply, State};
handle_call({read, Id}, _From, State) ->
  Reply = mnesia:dirty_read(employee, Id),
  {reply, Reply, State};
handle_call({update_sal, Id, Sal}, _From, State) ->
  Reply = case mnesia:dirty_read(employee, Id) of
            [] ->
              {error, employee_not_found};
            [EmpRec] ->
              mnesia:dirty_write(EmpRec#employee{sal=Sal})
          end,
  {reply, Reply, State};
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

handle_add(Id, Name, Dep, Sal) ->
  EmpAdd = fun() ->
               mnesia:write(#employee{id=Id,
                                      name=Name,
                                      dep=Dep,
                                      sal=Sal})
           end,
  mnesia:transaction(EmpAdd).
