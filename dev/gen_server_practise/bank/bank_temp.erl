-module(bank_temp).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start/0,
stop/0,
new/1,
credit/2,
debit/2,
balance/1,
close/1,
transfer/3]).

-export([init/1,
handle_call/3,
handle_cast/2,
handle_info/2,
terminate/2,
code_change/3]).


start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

new(CustomerName) ->
    gen_server:call(?MODULE, {new_account, CustomerName}).

credit(CustomerName, Amount) ->
    gen_server:call(?MODULE, {credit, CustomerName, Amount}).

debit(CustomerName, Amount) ->
    gen_server:call(?MODULE, {debit, CustomerName, Amount}).

balance(CustomerName) ->
    gen_server:call(?MODULE, {account_balance, CustomerName}).

close(CustomerName) ->
    gen_server:call(?MODULE, {close_account, CustomerName}).

transfer(FromCustomer, Amount, ToCustomer) ->
    gen_server:call(?MODULE, {transfer_amount, FromCustomer, Amount, ToCustomer}).


init([]) ->
    {ok, #{}}.


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



handle_call({credit, CustomerName, Amount}, _From, Accounts) ->
Reply = case maps:find(CustomerName, Accounts) of
{ok, Balance} ->
    NewBalance = Balance + Amount,
    AccountsUpdated = maps:update(CustomerName, NewBalance, Accounts),
    {ok, CustomerName, new_balance_is, NewBalance};
error ->
    AccountsUpdated = Accounts,
    {error, invalid_account}
end,
{reply, Reply, AccountsUpdated};


handle_call({debit, CustomerName, Amount}, _From, Accounts) ->
    case handle_debit(CustomerName, Account) of
        {ok, BalanceU} ->
            {reply, Reply, Accounts#{} }
        Error ->
            {reply, Error, StateU};

handle_call({account_balance, CustomerName, Amount}, _From, Accounts) ->
Reply = case maps:find(CustomerName, Accounts) of
    {ok, Balance} when Balance >= Amount ->
        NewBalance = Balance - Amount,
        AccountsUpdated = maps:update(CustomerName, NewBalance, Accounts),
        {ok, CustomerName, new_balance_is, NewBalance};
    {ok, Balance} ->
        AccountsUpdated = Accounts,
        {error, low_balance, Balance};
    error ->
        AccountsUpdated = Accounts,
        {error, invalid_account}
    end,
    {reply, Reply, AccountsUpdated};
    
handle_call({account_balance, CustomerName}, _From, Accounts) ->
    case maps:find(CustomerName, Account) of
        error ->
            {reply, {error, invalid_account}, Accounts};
        {ok, Balance} ->
            {reply, {ok, Balance}, Accounts}
    end;
handle_call({close_account, CustomerName}, _From, #{CustomerName := Balance} = Accounts)
    when Balance > 0 ->
        {reply, {error, account_not_empty}, Accounts};
handle_call({close_account, CustomerName}, _From, #{CustomerName := _Balance} = Accounts) ->
    AccountsUpdated = map:remove(CustomerName, Accounts),
    {reply, ok, AccountsUpdated};
handle_call({close_account, _CustomerName}, _From, Accounts) ->
    {reply, {error, invalid_account}, Accounts};

handle_call({transfer_amount, FromCustomer, Amount, ToCustomer}, _From,
                    #{FromCustomer := FromBalance, ToCustomer := ToBalance} = Accounts) ->
    if FromBalance < Amount ->
        {reply, {error, insufficient_balance}, Accounts};
    true ->
        FromBalanceUpdated = FromBalance - Amount,
        ToBalanceU = ToBalance + Amount,
        Accounts#{FromCustomer => FromBalanceUpdated, ToCustomer => ToBalance},
        {reply, {ok, FromBalaneU, ToBalanceU}, Accounts}
    end;
handle_call({transfer_amount, FromCustomer, _Amount, _ToCustomer}, _From,
                                        #{FromCustomer := _FromBalance} = Accounts) ->
    {reply, {error, to_account_does_not_exist}, Accounts};
handle_call({transfer_amount, _FromCustomer, _Amount, _ToCustomer}, _From, Accounts) ->
    {reply, {error, from_account_does_not_exist}, Accounts};

handle_call(stop, _From, Accounts) ->
    {stop, normal, stopped, Accounts}.

handle_cast(_Msg, Accounts) -> {noreply, Accounts}.

handle_info(_Info, Accounts) -> {noreply, Accounts}.

code_change(_Old_version, Accounts, _Extra) -> {ok, Accounts}.

terminate(_Reason, _Accounts) -> ok.
