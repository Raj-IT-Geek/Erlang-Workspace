-module(lottery).
-export([start/1,
         server/1,
         pick/1]).

start(Winner) ->
  Pid = spawn(lottery, server, [Winner]),
  register(lottery, Pid).

server(Winner) ->
  receive
    Winner ->
      io:format("You have won million dollars~n");
    Loser ->
      io:format("Sorry ~p is not a winner~n",
                [Loser]),
      server(Winner)
  after 60000 ->
          io:format("Lottery timed out~n")
  end.

pick(Ticket) ->
  lottery ! Ticket.
