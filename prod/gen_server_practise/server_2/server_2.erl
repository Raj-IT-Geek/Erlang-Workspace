-module(server_2).
-export([start/2, rpc/2]).

start(Server, Module) ->
    register(Server, spawn(fun() -> loop(Server, Module, Module:init()) end)).


rpc(Server, Request) ->
    Server ! {self(), Request},
    receive
        {Server, crash} -> exit(rpc);
        {Server, ok, Response} -> Response
    end.

loop(Server, Module, Old_state) ->
    receive
        {From, Request} ->
            try Module:handle(Request, Old_state) of
                {Response, New_state} ->
                    From ! {Server, ok, Response},
                    loop(Server, Module, New_state)
            catch
                _:Why ->
                    log_the_error(Server, Request, Why),
                    From ! {Server, crash},
                    loop(Server, Module, Old_state)
            end
    end.

log_the_error(Server, Request, Why) ->
    io:format("Server ~p request ~p ~ncaused exception ~p~n", [Server, Request, Why]).
