-module(broadcast).
-export([start/2, loop/0]).

%create N processes and broadcast a Message to all of them.

start(N, Message) ->
  ProcessList = create([], N),
  broadcast(ProcessList, Message),
  stop(ProcessList).


create(ProcessList, 0) -> ProcessList;
create(ProcessList, N) -> 
  create( [spawn(?MODULE, loop, []) | ProcessList], N-1 ).

broadcast([], Message) -> [];
broadcast([Pid|ProcessList], Message) -> 
  Pid ! Message,
  broadcast(ProcessList, Message).

stop([]) -> ok;
stop([Pid|ProcessList]) -> 
  Pid ! stop,
  stop(ProcessList).

loop() ->
  receive
    stop -> ok;
    Message -> io:format("~n~p", [Message]) 
  end.