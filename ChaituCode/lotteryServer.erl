-module(lotteryServer).
-behaviour(gen_server).

-export([start/1, pick/1, init/1,
         handle_call/3, terminate/2]).

start(Winner) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [Winner], []).

pick(Ticket) ->
  gen_server:call(?MODULE, Ticket).

init([Winner]) ->
  {ok, Winner}.

handle_call(Winner, _From, Winner) ->
  io:format("You have won a million dollars~n"),
  {stop, normal, Winner};
handle_call(_Ticket, _From, Winner) ->
  io:format("You are a loser~n"),
  {reply, ok, Winner}.

terminate(_Reason, _State) ->
  ok.
