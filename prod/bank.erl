-module(bank).
-export([start/0, loop/0, stop/0, create/1, debit/2, credit/2, balance/1]).

start() ->
    register(?MODULE, spawn(?MODULE, loop, [])).

stop() ->
    ?MODULE ! {stop}.
    get_reply().

create(AccountNo) ->
    ?MODULE ! {create, self(), AccountNo},
    get_reply().

debit(Amount, AccountNo) ->
    ?MODULE ! {debit, self(), Amount, AccountNo},
    get_reply().

credit(Amount, AccountNo) ->
    ?MODULE ! {credit, self(), Amount, AccountNo},
    get_reply().

balance(AccountNo) ->
    ?MODULE ! {balance, self(), AccountNo},
    get_reply().




get_reply() ->
    receive
        Reply -> Reply
    after 3000 ->
        timeout
    end.

loop() ->
    loop(#{}).

loop(Accounts) ->
    receive

        {create, From, AccountNo} ->
            case maps:find(AccountNo, Accounts) of
                error ->
                    From ! ok,
                    loop(Accounts#{AccountNo => 0});
                {ok, _Balance} ->
                    From ! {error, "already exists"},
                    loop(Accounts)
                end;

        {debit, From, Amount, AccountNo} ->
            case maps:find(AccountNo, Accounts) of
                error ->
                    From ! {error, "no account"},
                    loop(Accounts);
                {ok, Balance} when Balance >= Amount ->
                    FinalBalance = Balance - Amount,
                    From ! {ok, FinalBalance},
                    loop(Accounts#{AccountNo => FinalBalance});
                {ok, Balance} ->
                    From ! {error, {"low balance", Balance}},
                    loop(Accounts)
                end;

        {credit, From, Amount, AccountNo} ->
            case maps:find(AccountNo, Accounts) of
                error ->
                    From ! {error, "no account"},
                    loop(Accounts);
                {ok, Balance} ->
                    FinalBalance = Balance + Amount,
                    From ! {ok, FinalBalance},
                    loop(Accounts#{AccountNo => FinalBalance})
                end;

        {balance, From, AccountNo} ->
            case maps:find(AccountNo, Accounts) of
                error ->
                    From ! {error, "no account"},
                    loop(Accounts);
                {ok, Balance} ->
                    From ! {ok, Balance},
                    loop(Accounts)
                end;

        {stop} ->
            io:format("~n Bank Stopped! ~n~n")
            %loop(Accounts)

    end.



%2> c(bank).
%{ok,bank}
%3> bank:start().
%true
%4> bank:create(4).
%ok
%5> whereis(bank).
%<0.68.0>
