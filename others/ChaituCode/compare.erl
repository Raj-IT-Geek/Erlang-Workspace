-module(compare).

-export([dict/1, map/1]).

dict(N) ->
  Dict = dict(N, dict:new()),
  timer:tc(dict, find, [rand:uniform(N), Dict]).

dict(0, Dict) ->
  Dict;
dict(N, Dict) ->
  dict(N-1, dict:store(N, rand:uniform(1000), Dict)).

map(N) ->
  Map = map(N, #{}),
  timer:tc(maps, find, [rand:uniform(N), Map]).

map(0, Map) ->
  Map;
map(N, Map) ->
  map(N-1, Map#{N => rand:uniform(1000)}).
