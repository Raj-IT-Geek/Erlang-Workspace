-module(guessgame).
-export([start/1, game/1]).

start(Limit) ->
	Pick = rand:uniform(Limit),
	register(?MODULE, spawn(?MODULE, game, [Pick])).

game(Pick) ->
  receive

	Guess when Guess == Pick ->
      io:format("Hey you won~n");

    Guess when Guess > Pick ->
      io:format("try lower~n"),
      game(Pick);

    _ ->
      io:format("try higher~n"),
      game(Pick)

  end.
