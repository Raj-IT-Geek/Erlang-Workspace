%%%-------------------------------------------------------------------
%%% @author danny
%%% @copyright (C) 2016, danny
%%% @doc
%%%
%%% @end
%%% Created : 2016-06-08 13:09:07.882692
%%%-------------------------------------------------------------------
-module(mnesia_maps).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_table/2,
         put_record/2,
         get_record/2]).

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

create_table(Name, Fields) ->
  gen_server:call(?SERVER, {create_table, Name, Fields}).

put_record(Name, Map) ->
  gen_server:call(?SERVER, {put_record, Name, Map}).

get_record(Name, Key) ->
  gen_server:call(?SERVER, {get_record, Name, Key}).

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
  {ok, dict:new()}.

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
handle_call({create_table, Name, Fields}, _From, State) ->
  {Reply, NewState} = handle_create_table(Name, Fields, State),
  {reply, Reply, NewState};
handle_call({get_record, Name, Key}, _From, State) ->
  Reply = handle_get_record(Name, Key, State),
  {reply, Reply, State};
handle_call({put_record, Name, Map}, _From, State) ->
  Reply = handle_put_record(Name, Map, State),
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

handle_create_table(Name, Fields, State) ->
  Reply = mnesia:create_table(Name, [{attributes, Fields}]),
  NewState = dict:store(Name, Fields, State),
  {Reply, NewState}.

handle_put_record(Name, Map, State) ->
  Fields = dict:fetch(Name, State),
  Values = [maps:get(Field, Map, undefined) || Field <- Fields],
  Record = list_to_tuple([Name|Values]),
  mnesia:dirty_write(Record).

handle_get_record(Name, Key, State) ->
  case mnesia:dirty_read(Name, Key) of
    [] ->
      [];
    [Record] ->
      Fields = dict:fetch(Name, State),
      [_|Values] = tuple_to_list(Record),
      Zip = lists:zip(Fields, Values),
      Map = maps:from_list(Zip),
      Map#{'_table'=>Name}
  end.
