-module(server_3).
-export([start/2, rpc/2, swap_code/2]).


start(Server, Module) ->
    register(Server, spawn(fun() -> loop(Server, Module, Module:init()) end)).

swap_code(Server, New_module) -> rpc(Server, {swap_code, New_module}).

rpc(Server, Request) ->
    Server ! {self(), Request},
    receive
        {Server, Response} -> Response
    end.

loop(Server, Module, Old_state) ->
    receive
        {From, {swap_code, New_call_back_module}} ->
            From ! {Server, ack},
            loop(Server, New_call_back_module, Old_state);
        {From, Request} ->
            {Response, New_state} = Module:handle(Request, Old_state),
            From ! {Server, Response},
            loop(Server, Module, New_state)
    end.
