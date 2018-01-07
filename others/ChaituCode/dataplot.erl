%%%-------------------------------------------------------------------
%%% @author danny
%%% @copyright (C) 2017, danny
%%% @doc
%%%
%%% @end
%%% Created : 2017-01-12 12:32:46.062442
%%%-------------------------------------------------------------------
-module(dataplot).

-behaviour(gen_server).

-record(dataplot_sno, {
          key,
          value
         }).

-record(dataplot_entity, {
          sno,
          entity,
          child_entity
         }).

-record(dataplot_event, {
          sno,
          event
         }).

-record(dataplot, {
          sno,
          slot,
          entity,
          data
         }).

%% API
-export([start_link/0,
         notify/2,
         notify/3,
         read/2]).

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

notify(Entity, Event) ->
  notify(Entity, Event, erlang:localtime()).

notify(Entity, Event, DateTime) ->
  handle_notify(Entity, Event, DateTime).

read(Entity, Slot) ->
  handle_read(Entity, Slot).

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
  mnesia:create_table(dataplot_sno,
                      [{attributes, record_info(fields, dataplot_sno)}]),
  mnesia:create_table(dataplot_entity,
                      [{attributes, record_info(fields, dataplot_entity)},
                       {index, [entity]}]),
  mnesia:create_table(dataplot_event,
                      [{attributes, record_info(fields, dataplot_event)},
                       {index, [event]}]),
  mnesia:create_table(dataplot,
                      [{attributes, record_info(fields, dataplot)},
                       {index, [entity]}]),
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
terminate(Reason, State) ->
  io:format("Reason is ~p when state is ~p~n", [Reason, State]),
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

counter(Key) ->
  mnesia:dirty_update_counter(dataplot_sno, Key, 1).

datetime_to_dataplot({Date, Time}) ->
  Seconds = calendar:time_to_seconds(Time),
  {Date, Seconds div 60, Seconds rem 60}.

entity_to_sno(Entity) ->
  case get_entity_sno(Entity) of
    false ->
      set_entity_sno(Entity);
    Sno ->
      Sno
  end.

get_entity_sno({Entity, ChildEntity}) ->
  EntityRecs = mnesia:dirty_index_read(dataplot_entity, Entity, entity),
  case lists:keyfind(ChildEntity, #dataplot_entity.child_entity, EntityRecs) of
    false ->
      false;
    EntityRec ->
      EntityRec#dataplot_entity.sno
  end;
get_entity_sno(Entity) ->
  get_entity_sno({Entity, undefined}).

set_entity_sno({Entity, ChildEntity}) ->
  Sno = counter(entity),
  mnesia:dirty_write(#dataplot_entity{sno=Sno, entity=Entity,
                                      child_entity=ChildEntity}),
  Sno;
set_entity_sno(Entity) ->
  set_entity_sno({Entity, undefined}).

event_to_sno(Event) ->
  case get_event_sno(Event) of
    [] ->
      set_event_sno(Event);
    [EventRec] ->
      EventRec#dataplot_event.sno
  end.

get_event_sno(Event) ->
  mnesia:dirty_index_read(dataplot_event, Event, event).

set_event_sno(Event) ->
  Sno = counter(event),
  mnesia:dirty_write(#dataplot_event{sno=Sno, event=Event}),
  Sno.

sno_to_event(Sno) ->
  case mnesia:dirty_read(dataplot_event, Sno) of
    [] -> undefined;
    [EventRec] -> EventRec#dataplot_event.event
  end.

handle_notify(Entity, Event, DateTime) ->
  {_Date, Slot, Mark} = datetime_to_dataplot(DateTime),
  EntitySno = entity_to_sno(Entity),
  EventSno = event_to_sno(Event),
  EntityPlots = mnesia:dirty_index_read(dataplot, EntitySno, entity),
  case lists:keyfind(Slot, #dataplot.slot, EntityPlots) of
    false ->
      mnesia:dirty_write(#dataplot{sno = counter(dataplot),
                                   slot = Slot,
                                   entity = EntitySno,
                                   data = << Mark, EventSno:16 >>});
    #dataplot{data=Data} = EntitySlot ->
      mnesia:dirty_write(EntitySlot#dataplot{
                           data= <<Mark, EventSno:16, Data/binary >>})
  end.

handle_read(Entity, Slot) ->
  case get_entity_sno(Entity) of
    false ->
      [];
    EntitySno ->
      EntityRecs = mnesia:dirty_index_read(dataplot, EntitySno, entity),
      case lists:keyfind(Slot, #dataplot.slot, EntityRecs) of
        false ->
          [];
        Rec ->
          parse_data(Rec#dataplot.data)
      end
  end.

parse_data(BinaryData) ->
  parse_data(BinaryData, []).

parse_data(<< >>, DataList) ->
  DataList;
parse_data(<< Mark, EventSno:16, Rest/binary >>, DataList) ->
  parse_data(Rest, [{Mark, sno_to_event(EventSno)}|DataList]).
