-module(toll).
-export([start/0, stop/0, enter/1, leave/1, status/0, server_loop/0]).

start() ->
    register(toll_server, spawn(?MODULE, server_loop, [])),
	toll_server ! {self(), create_register},
	get_response().

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
	server_loop({}).

server_loop(Toll_Register) ->
    receive

        {From, create_register} ->
			Toll_Register_1 = {men, 0, women, 0, children, 0},
			From ! {"Toll Server started & Register Initialized!", Toll_Register_1},
            server_loop(Toll_Register_1);


		{From, enter, Human_type} ->
			{men, X, women, Y, children, Z} = Toll_Register,
			case Human_type of
				man -> Toll_Register_1 = {men, X+1, women, Y, children, Z};
				woman -> Toll_Register_1 = {men, X, women, Y+1, children, Z};
				child -> Toll_Register_1 = {men, X, women, Y, children, Z+1}
			end,
			From ! {ok, Toll_Register_1},
			server_loop(Toll_Register_1);


		{From, leave, Human_type} ->
			{men, X, women, Y, children, Z} = Toll_Register,
			case Human_type of
				man -> Toll_Register_1 = {men, X-1, women, Y, children, Z};
				woman -> Toll_Register_1 = {men, X, women, Y-1, children, Z};
				child -> Toll_Register_1 = {men, X, women, Y, children, Z-1}
			end,
			From ! {ok, Toll_Register_1},
			server_loop(Toll_Register_1);


        {From, current_status} ->
            From ! {ok, Toll_Register},
            server_loop(Toll_Register);

        {From, stop} ->
			From ! "Toll Server Stopped! ",
			void

    end.
