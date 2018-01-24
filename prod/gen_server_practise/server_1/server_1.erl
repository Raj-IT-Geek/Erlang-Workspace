-module(server_1).
-export([start/2, rpc/2]).

start(Server, Module) ->
    register(Server, spawn(fun() -> loop(Server, Module, Module:init()) end)).


rpc(Server, Request) ->
    Server ! {self(), Request},
    receive
        {Server, Response} -> Response
    end.

loop(Server, Module, State) ->
    receive
        {From, Request} ->
            {Response, State_1} = Module:handle(Request, State),
            From ! {Server, Response},
            loop(Server, Module, State_1)
    end.
