-module(guessgame).
-export([start/1, game/1, guess/1]).

-spec start(integer()) -> ok.
start(MaxLimit) ->
  register(guessgame, spawn(?MODULE, game, [MaxLimit])).

-spec guess(integer()) -> integer().
guess(Guess) ->
  guessgame ! Guess.

-spec game(integer()) -> ok.
game(MaxLimit) ->
  Chosen = rand:uniform(MaxLimit),
  io:format("I have picked a number~n"),
  listen(Chosen).

-spec listen(integer()) -> ok.
listen(Chosen) ->
  receive
    Guess when Guess == Chosen ->
      io:format("WOW! you have guessed the correct number~n");
    Guess when Guess > Chosen ->
      io:format("OOPS! my number is smaller~n"),
      listen(Chosen);
    _ ->
      io:format("OOPS! my number is bigger~n"),
      listen(Chosen)
  after 30000 ->
          io:format("Game timedout!!!~n")
  end.
