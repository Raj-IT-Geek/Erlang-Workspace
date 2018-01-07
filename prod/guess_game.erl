-module(guess_game).
-export([start_server/0, stop_server/0, new_game/1, my_guess/1, server_loop/0, game_loop/1]).

start_server() ->
	register(game_server, spawn(?MODULE, server_loop, [])).

stop_server() ->
    game_server ! {self(), stop},
	get_reply().

new_game(Max_Range) ->
	game_server ! {self(), new, Max_Range},
	get_reply().

my_guess(Guessed_Number) ->
	current_game ! {self(), guess, Guessed_Number},
	get_reply().

get_reply() ->
    receive
        Reply -> Reply
    after 2000 ->
        ok
    end.


server_loop() ->
	receive

		{From, new, Max_Range} ->
			Random_Number = rand:uniform(Max_Range),
			%io:format("~n ~n Random Num... ~p~n~n", [Random_Number]),
			register(current_game, spawn(?MODULE, game_loop, [Random_Number])),
			From ! {ok, "Random Number Generated"},
			server_loop();

		{From, stop} ->
			From ! "Game Server Stopped! ",
			void

	end.


game_loop(Random_Number) ->
	receive

		{From, guess, Guessed_Number} when Guessed_Number == Random_Number ->
			From ! "Hey you won!";

		{From, guess, Guessed_Number} when Guessed_Number > Random_Number ->
			From ! "Try a smaller number.",
			game_loop(Random_Number);

		{From, guess, Guessed_Number} when Guessed_Number < Random_Number ->
			From ! "Try a bigger number. ",
			game_loop(Random_Number)

	end.
