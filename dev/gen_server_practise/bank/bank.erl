-module(bank).
-behaviour(gen_server).
-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile(export_all).
-define(SERVER, ?MODULE).

start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

new_account(Customer_name)      -> gen_server:call(?MODULE, {new, Customer_name}).
credit(Customer_name, Amount)  -> gen_server:call(?MODULE, {credit, Customer_name, Amount}).
debit(Customer_name, Amount) -> gen_server:call(?MODULE, {debit, Customer_name, Amount}).

init([]) ->
    {ok, #{}}.

handle_call({new, Customer_name}, _From, Accounts) ->
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
                    Accounts1 = Accounts#{Customer_name => New_balance},
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
                    Accounts1 = Accounts#{Customer_name => New_balance},
                    {ok, Customer_name, new_balance_is, New_balance};
                {ok, Balance} ->
                    Accounts1 = Accounts,
                    {error, low_balance, Balance};
                error ->
                    Accounts1 = Accounts,
                    {error, invalid_account}
            end,

    {reply, Reply, Accounts1};


handle_call(stop, _From, Accounts)   ->
    {stop, normal, stopped, Accounts}.


handle_cast(_Msg, Accounts)        -> {noreply, Accounts}.

handle_info(_Info, Accounts)       -> {noreply, Accounts}.

code_change(_Old_version, Accounts, _Extra) -> {ok, Accounts}.

terminate(_Reason, _Accounts) -> ok.
