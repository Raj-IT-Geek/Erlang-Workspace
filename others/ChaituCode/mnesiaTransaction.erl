-module(mnesiaTransaction).

-record(bidding, {
          item,
          bid
         }).

-define(TOPUP, 80000).
-define(MIN, 10000).

-compile(export_all).

create() ->
  mnesia:create_table(bidding, [{attributes, record_info(fields, bidding)}]).

insert(Item) ->
  Bid = ?MIN + rand:uniform(?MIN),
  mnesia:dirty_write(#bidding{item = Item, bid = Bid}).

auction(Count, Time, Item) ->
  lists:foreach(fun(_X) ->
                    timer:apply_after(Time, ?MODULE, bidder, [Item])
                end,
                lists:seq(1, Count)).

bidder(Item) ->
  Bid = ?MIN + rand:uniform(?TOPUP),
  MnesiaUpdate = fun() ->
                     [Bidding] = mnesia:read(bidding, Item),
                     if
                       Bid > Bidding#bidding.bid ->
                         mnesia:write(Bidding#bidding{bid = Bid});
                       true ->
                         ok
                     end
                 end,
  mnesia:transaction(MnesiaUpdate).

win(Item) ->
  mnesia:dirty_read(bidding, Item).
