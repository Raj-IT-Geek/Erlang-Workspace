-module(my_bank).
-behaviour(gen_server).
-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile(export_all).
-define(SERVER, ?MODULE).

start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

new_account(Who)      -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount)  -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).

init([]) -> {ok, ets:new(?MODULE,[])}.

handle_call({new,Who}, _From, Customer_table) ->
    Reply = case ets:lookup(Customer_table, Who) of
                []  -> ets:insert(Customer_table, {Who,0}),
                       {welcome, Who};
                [_] -> {Who, you_already_are_a_customer}
            end,
    {reply, Reply, Customer_table};

handle_call({add, Who, X}, _From, Customer_table) ->
    Reply = case ets:lookup(Customer_table, Who) of
                []  -> not_a_customer;
                [{Who, Balance}] ->
                    New_balance = Balance + X,
                    ets:insert(Customer_table, {Who, New_balance}),
                    {thanks, Who, your_balance_is,  New_balance}
            end,
    {reply, Reply, Customer_table};

handle_call({remove, Who, X}, _From, Customer_table) ->
    Reply = case ets:lookup(Customer_table, Who) of
                [] -> not_a_customer;
                [{Who, Balance}] when X =< Balance ->
                    New_balance = Balance - X,
                    ets:insert(Customer_table, {Who, New_balance}),
                    {thanks, Who, your_balance_is,  New_balance};
                [{Who, Balance}] ->
                    {sorry, Who, you_only_have, Balance, in_the_bank}
            end,
{reply, Reply, Customer_table};

handle_call(stop, _From, Customer_table)   -> {stop, normal, stopped, Customer_table}.

handle_cast(_Msg, State)        -> {noreply, State}.

handle_info(_Info, State)       -> {noreply, State}.

code_change(_Old_version, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
