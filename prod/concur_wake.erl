-module(concur_wake).
-export([lazy_boy/0, wake/0, wakeit/0, wakethewaker/0]).

wakethewaker() ->
    spawn(?MODULE, wake, []).

wakeit() ->
    register(danny, spawn(concur_wake, lazy_boy, [])).

wake() ->
    process_flag(trap_exit, true),
    spawn_link(concur, lazy_boy, []),
    receive
        Whatever ->
            io:format("~p~n", [Whatever])
    end.

lazy_boy() ->
    io:format("Ok I am back. Tell me what? ~n"),
    receive
        {X, Y} ->
            io:format("X  Y is ~p ~p ~n", [X, Y]),
            lazy_boy();
        Whatever ->
            io:format("You said ~p~n", [Whatever]),
            lazy_boy()
    after 10000 ->
        io:format("You said nothing ~n")
    end.
