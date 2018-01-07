%%% Module to count the number of vowels and consonants in a sentence.

-module(count_vows_cons).
-export([count_vows_cons/1]).

count_vows_cons([Str]) -> count_vows_cons(Str, 0, 0).

count_vows_cons([], Vows, Cons) -> Vows, Cons,
Vowels = ['a', 'e', 'i', 'o', 'u'].

count_vows_cons([H|T], Vows, Cons) ->
    if member(H, Vowels) -> count_vows_cons([T], Vows+1, Cons),
    true -> count_vows_cons([T], Vows, Cons+1);
