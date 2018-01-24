-module(bank_imperative).
-behaviour(gen_server).
-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile(export_all).
-define(SERVER, ?MODULE).

start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

new(Customer_name)              -> gen_server:call(?MODULE, {new_account, Customer_name}).
credit(Customer_name, Amount)   -> gen_server:call(?MODULE, {credit, Customer_name, Amount}).
debit(Customer_name, Amount)    -> gen_server:call(?MODULE, {debit, Customer_name, Amount}).
balance(Customer_name)          -> gen_server:call(?MODULE, {account_balance, Customer_name}).
close(Customer_name)            -> gen_server:call(?MODULE, {close_account, Customer_name}).
transfer(From_customer, Amount,
                    To_customer)-> gen_server:call(?MODULE, {transfer_amount, From_customer, Amount, To_customer}).


init([]) ->
    {ok, maps:new()}.

handle_call({new_account, Customer_name}, _From, Accounts) ->
    Reply = case maps:find(Customer_name, Accounts) of
                {ok, _Balance} ->
                    Accounts1 = Accounts,
                    {error, Customer_name, account_already_exists};
                error ->
                    Accounts1 = Accounts#{Customer_name => 0},
                    {ok, Customer_name, account_created}
            end,
    {reply, Reply, Accounts1};

handle_call({credit, Customer_name, Amount}, _From, Accounts) ->
    Reply = case maps:find(Customer_name, Accounts) of
                {ok, Balance} ->
                    New_balance = Balance + Amount,
                    Accounts1 = maps:update(Customer_name, New_balance, Accounts),
                    {ok, Customer_name, new_balance_is, New_balance};
                error ->
                    Accounts1 = Accounts,
                    {error, invalid_account}
            end,

    {reply, Reply, Accounts1};


handle_call({debit, Customer_name, Amount}, _From, Accounts) ->
    Reply = case maps:find(Customer_name, Accounts) of
                {ok, Balance} when Balance >= Amount ->
                    New_balance = Balance - Amount,
                    Accounts1 = maps:update(Customer_name, New_balance, Accounts),
                    {ok, Customer_name, new_balance_is, New_balance};
                {ok, Balance} ->
                    Accounts1 = Accounts,
                    {error, low_balance, Balance};
                error ->
                    Accounts1 = Accounts,
                    {error, invalid_account}
            end,

    {reply, Reply, Accounts1};


handle_call({account_balance, Customer_name}, _From, Accounts) ->
    Reply = case maps:find(Customer_name, Accounts) of
                {ok, Balance} ->
                    {ok, Customer_name, balance_is, Balance};
                error ->
                    {error, invalid_account}
            end,
    {reply, Reply, Accounts};


handle_call({close_account, Customer_name}, _From, Accounts) ->
    Reply = case maps:find(Customer_name, Accounts) of
                {ok, Balance} when Balance > 0 ->
                    Accounts1 = Accounts,
                    {error, Customer_name, please_withdraw_amount, Balance};
                {ok, _Balance}  ->
                    Accounts1 = maps:remove(Customer_name, Accounts),
                    {ok, account_is_closed_for, Customer_name};
                error ->
                    Accounts1 = Accounts,
                    {error, invalid_account}
            end,
    {reply, Reply, Accounts1};


handle_call({transfer_amount, From_customer, Amount, To_customer}, _From, Accounts) ->
    Reply = case maps:find(To_customer, Accounts) of
                error ->
                    Accounts1 = Accounts,
                    {error, To_customer, account_does_not_exist};
                {ok, To_customer_balance} ->
                    case maps:find(From_customer, Accounts) of
                        error ->
                            Accounts1 = Accounts,
                            {error, From_customer, account_does_not_exist};
                        {ok, From_customer_balance} when From_customer_balance >= Amount ->
                            Accounts2 = maps:update(From_customer, From_customer_balance - Amount, Accounts),
                            Accounts1 = maps:update(To_customer, To_customer_balance + Amount, Accounts2),
                            {transferred, Amount, from, From_customer, to, To_customer};
                        {ok, Balance} ->
                            Accounts1 = Accounts,
                            {error, insufficient_balance, Balance, in_account, From_customer}
                    end
            end,
            {reply, Reply, Accounts1};

handle_call(stop, _From, Accounts)   ->
    {stop, normal, stopped, Accounts}.


handle_cast(_Msg, Accounts)        -> {noreply, Accounts}.

handle_info(_Info, Accounts)       -> {noreply, Accounts}.

code_change(_Old_version, Accounts, _Extra) -> {ok, Accounts}.

terminate(_Reason, _Accounts) -> ok.
