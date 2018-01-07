-module(guess_game).
-export([start/1, my_guess/1, loop/1]).

start(Max_Range) ->
	Random_Number = rand:uniform(Max_Range),
	register(game_server, spawn(?MODULE, loop, [Random_Number])).

my_guess(Guessed_Number) ->
	game_server ! {self(), guess, Guessed_Number},
	get_reply().

get_reply() ->
    receive
        Reply -> Reply
    after 2000 ->
        ok
    end.


loop(Random_Number) ->
	receive
		{From, guess, Guessed_Number} when Guessed_Number == Random_Number ->
			From ! "Hey you won!";

		{From, guess, Guessed_Number} when Guessed_Number > Random_Number ->
			From ! "Try a smaller number.",
			loop(Random_Number);

		{From, guess, Guessed_Number} when Guessed_Number < Random_Number ->
			From ! "Try a bigger number. ",
			loop(Random_Number)
	end.
