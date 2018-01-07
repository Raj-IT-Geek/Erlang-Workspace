-module(addbook).
-export([start_link/0,
         init/1,
         new_address/1]).

-record(addbook, {
          name,
          phone,
          city,
          relation
         }).

start_link() ->
  gen_provision:start_link({local, ?MODULE}, ?MODULE, []).

new_address(Address) ->
  gen_provision:create(?MODULE, addbook, Address).

init([]) ->
  ProvConf = [#{name => addbook,
                attributes => record_info(fields, addbook),
                storage_type => ram_copies,
                index => [ #addbook.phone,
                          #addbook.city ]}],
  {ok, ProvConf}.
