%%%-------------------------------------------------------------------
%%% @author danny
%%% @copyright (C) 2017, danny
%%% @doc
%%%
%%% @end
%%% Created : 2017-05-30 16:51:09.445595
%%%-------------------------------------------------------------------
-module(etswrapper).

-behaviour(gen_server).

%% API
-export([start_link/0
        ,create/1
        ,read/3
        ,sort/2
        ,clear/0]).

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

create(DataList) ->
  gen_server:call(?SERVER, {create, DataList}).

read(EtsId, Start, End) ->
  gen_server:call(?SERVER, {read, EtsId, Start, End}).

sort(EtsId, Fun) ->
  gen_server:call(?SERVER, {sort, EtsId, Fun}).

clear() ->
  gen_server:cast(?SERVER, clear).

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
handle_call({create, DataList}, _From, State) ->
  EtsId = ets:new(cache, [compressed]),
  lists:foreach(
    fun(N) ->
        ets:insert(EtsId, {N, lists:nth(N, DataList)})
    end,
    lists:seq(1, length(DataList))
   ),
  {reply, EtsId, State#{EtsId => erlang:system_time(second)}};
handle_call({read, EtsId, Start, End}, _From, State) ->
  case maps:find(EtsId, State) of
    error ->
      {reply, {error, not_found}, State};
    {ok, _LastTime} ->
      Reply = lists:foldr(
                fun(Key, DataList) ->
                    case ets:lookup(EtsId, Key) of
                      [] ->
                        DataList;
                      [{Key, Data}] ->
                        [Data|DataList]
                    end
                end,
                [],
                lists:seq(Start, End)
               ),
      {reply, {ok, Reply}, State#{EtsId => erlang:system_time(second)}}
  end;
handle_call({sort, EtsId, Fun}, _From, State) ->
  case maps:find(EtsId, State) of
    error ->
      {reply, {error, not_found}, State};
    {ok, _LastTime} ->
      ValueList = [ V || {_K, V} <- ets:tab2list(EtsId) ],
      ValueListSort = lists:sort(Fun, ValueList),
      lists:foreach(
        fun(N) ->
            ets:insert(EtsId, {N, lists:nth(N, ValueListSort)})
        end,
        lists:seq(1, length(ValueListSort))
       ),
      {reply, ok, State#{EtsId => erlang:system_time(second)}}
  end;
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
handle_cast(clear, State) ->
  io:format("State now is ~p~n", [State]),
  TsNow = erlang:system_time(second),
  StateU = maps:fold(
             fun(EtsId, Ts, StateA) ->
                 if
                   TsNow - Ts > 15 * 60 ->
                     io:format("Deleting ~p~n", [EtsId]),
                     ets:delete(EtsId),
                     StateA;
                   true ->
                     io:format("Diff is ~p~n", [TsNow - Ts]),
                     StateA#{EtsId => Ts}
                 end
             end,
             #{},
             State
            ),
  {noreply, StateU};
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





