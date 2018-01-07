-module(mylib).
-export([print/1]).

print(Term) -> 
    io:format("~p.~n", [Term]).

