-module(ageof).
-export([name/1,
         name1/1,
         name2/1]).

name(mahanthi) -> 35;
name(danny) -> 37;
name(ravi) -> 32;
name(_) -> 100.

name1(Name) ->
  case Name of
    mahanthi -> 35;
    danny -> 37;
    ravi -> 32;
    _ -> 100
  end.

name2(Name) ->
  if
    Name == mahanthi -> 35;
    Name == danny -> 37;
    Name == ravi -> 32;
    true -> 100
  end.
