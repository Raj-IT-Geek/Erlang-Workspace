-module(ggm). %guess game multiuser
-export([start_server/0, stop_server/0, new_user/1, new_game/2, guess/2, server_loop/0, game_loop/1]).

start_server() ->
	register(game_server, spawn(?MODULE, server_loop, [])).

stop_server() ->
    game_server ! {self(), stop},
	get_response().

new_user(New_user_name) ->
	game_server ! {self(), new_user, New_user_name},
	get_response().

new_game(User_name, Max_range) ->
	game_server ! {self(), new_game, User_name, Max_range},
	get_response().

guess(User_name, Guessed_number) ->
	game_server ! {self(), guess, User_name, Guessed_number},
	get_response().

get_response() ->
    receive
        Response -> Response
    after 1000 ->
        ok
    end.

server_loop() -> server_loop(#{}).

server_loop(User_db) ->
	receive

		{From, new_user, New_user_name} ->
			case maps:find(New_user_name, User_db) of
                error ->
                    From ! {ok, "User Created"},
					          User_db1 = User_db#{New_user_name => 0},
                    server_loop(User_db1);
                {ok, _X} ->
                    From ! {error, "user already exists"},
                    server_loop(User_db)
                end;

		{From, new_game, User_name, Max_range} ->
			case maps:find(User_name, User_db) of
				error ->
					From ! {error, "User not found."},
					server_loop(User_db);
				{ok, _X} ->
					From ! {ok, "new game started."},
					Random_number = rand:uniform(Max_range),
					New_pid = spawn(?MODULE, game_loop, [Random_number]),
					User_db1 = User_db#{User_name => New_pid},
					%io:format("~nPID: ~p, ~nUser_db: ~p~n", [New_pid, User_db1]),
					server_loop(User_db1)
				end;


		{From, guess, User_name, Guessed_number} ->
			case maps:find(User_name, User_db) of
				error ->
					From ! {error, "User not found."},
					server_loop(User_db);
				{ok, _Y} ->
					#{User_name := User_pid} = User_db,
					User_pid ! {From, Guessed_number},
					server_loop(User_db)
				end;

		{From, stop} ->
			From ! "Game Server Stopped! ",
			void

	end.


game_loop(Random_number) ->
	receive

		{From, Guessed_number} when Guessed_number == Random_number ->
			From ! "Hey you won!";

		{From, Guessed_number} when Guessed_number > Random_number ->
			From ! "Try a smaller number.",
			game_loop(Random_number);

		{From, Guessed_number} when Guessed_number < Random_number ->
			From ! "Try a bigger number. ",
			game_loop(Random_number)

	end.



%ggh:start_server().
%ggm:new_user("Raj").
%ggm:new_game("Raj", 7).
%ggm:guess("Raj", 5).
