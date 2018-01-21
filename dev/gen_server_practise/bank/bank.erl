-module(bank).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-export([start/0,
        stop/0,
        new/1,
        credit/2,
        debit/2,
        balance/1,
        close/1,
        transfer/3]).

% Calling Functions
start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

new(CustomerName) ->
  gen_server:call(?MODULE, {new, CustomerName}).

credit(CustomerName, Amount) ->
  gen_server:call(?MODULE, {credit, CustomerName, Amount}).

debit(CustomerName, Amount) ->
  gen_server:call(?MODULE, {debit, CustomerName, Amount}).

balance(CustomerName) ->
  gen_server:call(?MODULE, {balance, CustomerName}).

close(CustomerName) ->
  gen_server:call(?MODULE, {close, CustomerName}).

transfer(FromCustomer, Amount, ToCustomer) ->
  gen_server:call(?MODULE, {transfer, FromCustomer, Amount, ToCustomer}).

init([]) ->
  {ok, #{}}.

% Callback Functions
handle_call({new, CustomerName}, _From, Accounts) ->
  new_account(CustomerName, Accounts);

handle_call({credit, CustomerName, Amount}, _From, Accounts) ->
  credit_account(CustomerName, Amount, Accounts);

handle_call({debit, CustomerName, Amount}, _From, Accounts) ->
  debit_account(CustomerName, Amount, Accounts);

handle_call({balance, CustomerName}, _From, Accounts) ->
  account_balance(CustomerName, Accounts);

handle_call({transfer, FromCustomer, Amount, ToCustomer}, _From, Accounts) ->
  transfer_amount(FromCustomer, Amount, ToCustomer, Accounts);

handle_call({close, CustomerName}, _From, Accounts) ->
  close_account(CustomerName, Accounts);

handle_call(stop, _From, Accounts) ->
    {stop, normal, stopped, Accounts}.

handle_cast(_Msg, Accounts) -> {noreply, Accounts}.

handle_info(_Info, Accounts) -> {noreply, Accounts}.

code_change(_Old_version, Accounts, _Extra) -> {ok, Accounts}.

terminate(_Reason, _Accounts) -> ok.



% Handling Functions
new_account(CustomerName, Accounts) ->
  Reply = case maps:find(CustomerName, Accounts) of
            {ok, _Balance} ->
                UpdatedAccounts = Accounts,
                {error, account_already_exists};
            error ->
                UpdatedAccounts = maps:put(CustomerName, 0, Accounts),
                {ok, account_created}
          end,
  {reply, Reply, UpdatedAccounts}.

account_balance(CustomerName, Accounts) ->
  Reply = case maps:find(CustomerName, Accounts) of
            {ok, Balance} ->
                {ok, Balance};
            error ->
                {error, invalid_account}
          end,
  {reply, Reply, Accounts}.
  
credit_account(CustomerName, Amount, Accounts) ->
  Reply = case maps:find(CustomerName, Accounts) of
            {ok, Balance} ->
                UpdatedAccounts = maps:update(CustomerName, Balance + Amount, Accounts),
                {ok, credited};
            error ->
                UpdatedAccounts = Accounts,
                {error, invalid_account}
          end,
{reply, Reply, UpdatedAccounts}.

debit_account(CustomerName, Amount, Accounts) ->
  Reply = case maps:find(CustomerName, Accounts) of
            {ok, Balance} when Balance >= Amount ->
                UpdatedAccounts = maps:update(CustomerName, Balance - Amount, Accounts),
                {ok, debited};
            {ok, _Balance} ->
                UpdatedAccounts = Accounts,
                {error, insufficient_funds};
            error ->
                UpdatedAccounts = Accounts,
                {error, invalid_account}
          end,
{reply, Reply, UpdatedAccounts}.

transfer_amount(FromCustomer, Amount, ToCustomer, Accounts) ->
  Reply = case maps:find(ToCustomer, Accounts) of
            error ->
                UpdatedAccounts = Accounts,
                {error, ToCustomer, invalid_account};
            {ok, ToBalance} ->
                case maps:find(FromCustomer, Accounts) of
                    error ->
                        UpdatedAccounts = Accounts,
                        {error, FromCustomer, invalid_account};
                    {ok, FromBalance} when FromBalance >= Amount ->
                        TempAccounts = maps:update(FromCustomer, FromBalance - Amount, Accounts),
                        UpdatedAccounts = maps:update(ToCustomer, ToBalance + Amount, TempAccounts),
                        {ok, transferred};
                    {ok, _Balance} ->
                        UpdatedAccounts = Accounts,
                        {error, insufficient_funds}
                end
          end,
  {reply, Reply, UpdatedAccounts}.

close_account(CustomerName, Accounts) ->
  Reply = case maps:find(CustomerName, Accounts) of
            {ok, Balance} when Balance > 0 ->
                UpdatedAccounts = Accounts,
                {error, account_not_empty};
            {ok, _Balance}  ->
                UpdatedAccounts = maps:remove(CustomerName, Accounts),
                {ok, account_closed};
            error ->
                UpdatedAccounts = Accounts,
                {error, invalid_account}
          end,
  {reply, Reply, UpdatedAccounts}.