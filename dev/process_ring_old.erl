-module(process_ring).
-export([start/3, loop/0]).

start(1, _, _) -> {error, "N should be atleast 2"};
start(N, Message, M) ->
  RootPid = spawn(?MODULE, loop, []),
  ProcessList = create([], RootPid, N-1, RootPid),
  broadcast(Message, M, ProcessList),
  stop(ProcessList).

create(ProcessList, RootPid, 0, PreviousPid) -> lists:reverse([{PreviousPid, RootPid} | ProcessList]);
create(ProcessList, RootPid, N, PreviousPid) -> 
  CurrentPid = spawn(?MODULE, loop, []),
  create( [ {PreviousPid, CurrentPid} | ProcessList ], RootPid, N-1, CurrentPid).

broadcast(Message, 0, ProcessList) -> [];
broadcast(Message, M, ProcessList) -> 
  cascade(Message, ProcessList),
  broadcast(Message, M-1, ProcessList).

cascade(Message, [{Pid1, Pid2}|ProcessList]) -> 
  Pid1 ! {Message, Pid2}
  broadcast(ProcessList, Message).

stop([]) -> ok;
stop([{Pid, _}|ProcessList]) -> 
  Pid ! stop,
  stop(ProcessList).

loop() ->

  receive
    stop -> stop;
    {Message, NextPid} -> 
        io:format("~n~p~n", [Message]),
        loop()
  end.
