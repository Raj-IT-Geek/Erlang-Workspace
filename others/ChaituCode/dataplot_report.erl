-module(dataplot_report).

-export([scan/1]).

scan(Entity) ->
  scan(Entity, 1, [], []).

scan(_Entity, 1440, _State, Rows) ->
  lists:reverse(Rows);
scan(Entity, Slot, State, Rows) ->
  Events = dataplot:read(Entity, Slot),
  case interpret(Events, State, Slot) of
    {[], NewState} ->
      scan(Entity, Slot + 1, NewState, Rows);
    {Row, NewState} ->
      scan(Entity, Slot + 1, NewState, [{Slot, Row}|Rows])
  end.

interpret(Events, State, Slot) ->
  interpret(Events, State, Slot, []).

interpret([], State, Slot, Row) ->
  interpret_state(State, Slot, Row, []);
interpret([{Mark, Event}|Events], State, Slot, Row) ->
  case event_type(Event) of
    counter ->
      RowU = case lists:keyfind(Event, 1, Row) of
               false ->
                 [{Event, 1}|Row];
               {_, C} ->
                 lists:keyreplace(Event, 1, Row, {Event, C+1})
             end,
      interpret(Events, State, Slot, RowU);
    {timer_start, EventTag} ->
      interpret(Events,
                [{EventTag, Slot, Mark}|State],
                Slot,
                Row);
    {timer_stop, EventTag} ->
      case lists:keytake(EventTag, 1, State) of
        false ->
          interpret(Events, State, Slot, Row);
        {value, {_, Slot, MarkStart}, NewState} ->
          interpret(Events,
                    NewState,
                    Slot,
                    [{EventTag, Mark - MarkStart}|Row]);
        {value, _, NewState} ->
          interpret(Events,
                    NewState,
                    Slot,
                    [{EventTag, Mark}|Row])
      end;
    {max_up, EventTag} ->
      case lists:keytake(EventTag, 1, State) of
        false ->
          interpret(Events,
                    [{EventTag, 1, 1}|State],
                    Slot,
                    Row);
        {value, {_, Now, Max}, NewState} ->
          NowU = Now + 1,
          MaxU = if NowU > Max -> NowU; true -> Max end,
          interpret(Events,
                    [{EventTag, NowU, MaxU}|NewState],
                    Slot,
                    Row)
      end;
    {max_down, EventTag} ->
      case lists:keytake(EventTag, 1, State) of
        false ->
          interpret(Events, State, Slot, Row);
        {value, {_, Now, Max}, NewState} when Now > 0 ->
          interpret(Events,
                    [{EventTag, Now-1, Max}|NewState],
                    Slot,
                    Row);
        {value, EventZero, NewState} ->
          interpret(Events, [EventZero|NewState], Slot, Row)
      end;
    _Other ->
      interpret(Events, State, Slot, Row)
  end.

interpret_state([], _Slot, Row, NewState) ->
  {Row, NewState};
interpret_state([{EventTag, A, B}|State], Slot, Row, NewState) ->
  case event_type(EventTag) of
    timer ->
      Value = if
                A == Slot -> 60 - B;
                true -> 60
              end,
      interpret_state(State,
                      Slot,
                      [{EventTag, Value}|Row],
                      [{EventTag, A, B}|NewState]);
    max when A == 0 ->
      interpret_state(State,
                      Slot,
                      [{EventTag, B}|Row],
                      NewState);
    max ->
      interpret_state(State,
                      Slot,
                      [{EventTag, B}|Row],
                      [{EventTag, A, A}|NewState]);
    _Other ->
      interpret_state(State, Slot, Row, [{EventTag, A, B}|NewState])
  end.

%% Counters
event_type("acd_call_arrive") -> counter;
event_type("acd_call_abandon") -> counter;
event_type("acd_call_answer") -> counter;
event_type("acd_call_end") -> counter;
event_type("queue_call_arrive") -> counter;
event_type("queue_call_abandon") -> counter;
event_type("queue_call_menu_0") -> counter;
event_type("queue_call_menu_1") -> counter;
event_type("queue_call_menu_2") -> counter;
event_type("queue_call_menu_3") -> counter;
event_type("queue_call_menu_4") -> counter;
event_type("queue_call_menu_5") -> counter;
event_type("queue_call_menu_6") -> counter;
event_type("queue_call_menu_7") -> counter;
event_type("queue_call_menu_8") -> counter;
event_type("queue_call_menu_9") -> counter;
event_type("queue_call_menu_*") -> counter;
event_type("queue_call_menu_#") -> counter;
event_type("queue_call_timeout") -> counter;
event_type("queue_call_toi") -> counter;
event_type("aq_answer") -> counter;
event_type("aq_missed") -> counter;
event_type("aq_bye") -> counter;
event_type("aq_xfer") -> counter;
event_type("aq_wrapup") -> counter;
%% Maximum values / Peak values
event_type("peak_wait") -> max;
event_type("peak_wait_up") -> {max_up, "peak_wait"};
event_type("peak_wait_down") -> {max_down, "peak_wait"};
event_type("peak_talk") -> max;
event_type("peak_talk_up") -> {max_up, "peak_talk"};
event_type("peak_talk_down") -> {max_down, "peak_talk"};
%% Timers
event_type("acd_wait") -> timer;
event_type("acd_wait_start") -> {timer_start, "acd_wait"};
event_type("acd_wait_stop") -> {timer_stop, "acd_wait"};
event_type("acd_talk") -> timer;
event_type("acd_talk_start") -> {timer_start, "acd_talk"};
event_type("acd_talk_stop") -> {timer_stop, "acd_talk"};
event_type("queue_wait") -> timer;
event_type("queue_wait_start") -> {timer_start, "queue_wait"};
event_type("queue_wait_stop") -> {timer_stop, "queue_wait"};
event_type("aq_talk") -> timer;
event_type("aq_talk_start") -> {timer_start, "aq_talk"};
event_type("aq_talk_stop") -> {timer_stop, "aq_talk"};
event_type("acd_wait_abandon") -> timer;
event_type("acd_wait_abandon_start") -> {timer_start, "acd_wait_abandon"};
event_type("acd_wait_abandon_stop") -> {timer_stop, "acd_wait_abandon"};
event_type("queue_wait_exit_abandon") -> timer;
event_type("queue_wait_exit_abandon_start") ->
  {timer_start, "queue_wait_exit_abandon"};
event_type("queue_wait_exit_abandon_stop") ->
  {timer_stop, "queue_wait_exit_abandon"};
event_type(Other) ->
  io:format("Unknown other ~p~n", [Other]),
  unknown.

