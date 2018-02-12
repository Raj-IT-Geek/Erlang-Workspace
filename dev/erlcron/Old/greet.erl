-module(greet).
-export([hello/1]).

hello([]) -> io:format("~nBye!~n");
hello([H|T]) -> 
  io:format("\nHello... ~p", [H]),
  hello(T).
