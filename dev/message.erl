-module(message).
-export([start/0, print/1, stop/0, loop/0]).

start() ->
  register(pid, spawn(message, loop, [])).

print(Message) ->
  pid ! {print, Message}.

stop() -> 
  pid ! stop.

loop() ->
  receive
    {print, Message} -> 
        io:format("~n~p~n", [Message]),
        loop();
    stop -> true
  end.
