-module(concur).
-export([lazy_boy/0, wake/0, wakeit/0]).

wakeit() ->
    register(danny, spawn(concur, lazy_boy, [])).

wake() ->
    spawn(concur, lazy_boy, []).

lazy_boy() ->
    io:format("Ok I am back. Tell me what? ~n"),
    receive
        {X, Y} ->
            io:format("X  Y is ~p ~p ~n", [X, Y]),
            lazy_boy();
        Whatever ->
            io:format("You said ~p ~n", [Whatever]),
            lazy_boy()
    after 10000 ->
        io:format("You said nothing ~n")
    end.
