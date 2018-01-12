-module(toll).
-export([start/0, stop/0, enter/1, leave/1, status/0, server_loop/0]).

start() ->
    register(toll_server, spawn(?MODULE, server_loop, [])).
	%get_response().

stop() ->
    toll_server ! {self(), stop},
    get_response().

enter(Human_Type) ->
    toll_server ! {self(), enter, Human_Type},
    get_response().

leave(Human_Type) ->
    toll_server ! {self(), leave, Human_Type},
    get_response().

status() ->
    toll_server ! {self(), current_status},
    get_response().


get_response() ->
    receive
        Reply -> Reply
    after 1000 ->
        timeout
    end.

server_loop() ->
	server_loop({men, 0, women, 0, children,0}).

server_loop({men, X, women, Y, children, Z}) ->
    receive

		{From, enter, man} ->
            From ! ok,
            server_loop({men, X+1, women, Y, children, Z});

        {From, enter, woman} ->
            From ! ok,
            server_loop({men, X, women, Y+1, children, Z});

        {From, enter, child} ->
            From ! ok,
            server_loop({men, X, women, Y, children, Z+1});


        {From, leave, man} ->
            From ! ok,
            server_loop({men, X-1, women, Y, children, Z});

        {From, leave, woman} ->
            From ! ok,
            server_loop({men, X, women, Y-1, children, Z});

        {From, leave, child} ->
            From ! ok,
            server_loop({men, X, women, Y, children, Z-1});


        {From, current_status} ->
            From ! {ok, {men, X, women, Y, children, Z}},
            server_loop({men, X, women, Y, children, Z});

        {From, stop} ->
			From ! "Toll Server Stopped! ",
			void

    end.
