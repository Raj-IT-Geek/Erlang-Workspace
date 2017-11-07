-module(numerology).
-export([name/1, hebrew/1, vedic/1, number/1]).

number(N) -> digit_sum(N, 0).

name(N) -> vedic(N).

hebrew(Name) ->
  digit_sum(hebrew(string:to_upper(Name), 0), 0).

hebrew([], Sum) -> Sum;
hebrew("CH"++Name, Sum) -> hebrew(Name, Sum + 8);
hebrew("PH"++Name, Sum) -> hebrew(Name, Sum + 80);
hebrew("SH"++Name, Sum) -> hebrew(Name, Sum + 300);
hebrew("TH"++Name, Sum) -> hebrew(Name, Sum + 400);
hebrew("TZ"++Name, Sum) -> hebrew(Name, Sum + 90);
hebrew("A"++Name, Sum) -> hebrew(Name, Sum + 1);
hebrew("B"++Name, Sum) -> hebrew(Name, Sum + 2);
hebrew("C"++Name, Sum) -> hebrew(Name, Sum + 20);
hebrew("D"++Name, Sum) -> hebrew(Name, Sum + 4);
hebrew("E"++Name, Sum) -> hebrew(Name, Sum + 5);
hebrew("F"++Name, Sum) -> hebrew(Name, Sum + 80);
hebrew("G"++Name, Sum) -> hebrew(Name, Sum + 3);
hebrew("H"++Name, Sum) -> hebrew(Name, Sum + 5);
hebrew("I"++Name, Sum) -> hebrew(Name, Sum + 10);
hebrew("J"++Name, Sum) -> hebrew(Name, Sum + 10);
hebrew("K"++Name, Sum) -> hebrew(Name, Sum + 20);
hebrew("L"++Name, Sum) -> hebrew(Name, Sum + 30);
hebrew("M"++Name, Sum) -> hebrew(Name, Sum + 40);
hebrew("N"++Name, Sum) -> hebrew(Name, Sum + 50);
hebrew("O"++Name, Sum) -> hebrew(Name, Sum + 70);
hebrew("P"++Name, Sum) -> hebrew(Name, Sum + 80);
hebrew("Q"++Name, Sum) -> hebrew(Name, Sum + 100);
hebrew("R"++Name, Sum) -> hebrew(Name, Sum + 200);
hebrew("S"++Name, Sum) -> hebrew(Name, Sum + 60);
hebrew("T"++Name, Sum) -> hebrew(Name, Sum + 9);
hebrew("U"++Name, Sum) -> hebrew(Name, Sum + 6);
hebrew("V"++Name, Sum) -> hebrew(Name, Sum + 6);
hebrew("W"++Name, Sum) -> hebrew(Name, Sum + 6);
hebrew("X"++Name, Sum) -> hebrew(Name, Sum + 60);
hebrew("Y"++Name, Sum) -> hebrew(Name, Sum + 10);
hebrew("Z"++Name, Sum) -> hebrew(Name, Sum + 7);
hebrew([_|Name], Sum) -> hebrew(Name, Sum + 0).

digit_sum(0, S) when S < 10 -> S;
digit_sum(0, S) -> digit_sum(S, 0);
digit_sum(N, S) -> digit_sum(N div 10, S + (N rem 10)).

vedic(Name) ->
  digit_sum(vedic(string:to_upper(Name), 0), 0).

vedic([], Sum) -> Sum;
vedic("A"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("B"++Name, Sum) -> vedic(Name, Sum + 2);
vedic("C"++Name, Sum) -> vedic(Name, Sum + 2);
vedic("D"++Name, Sum) -> vedic(Name, Sum + 4);
vedic("E"++Name, Sum) -> vedic(Name, Sum + 5);
vedic("F"++Name, Sum) -> vedic(Name, Sum + 8);
vedic("G"++Name, Sum) -> vedic(Name, Sum + 3);
vedic("H"++Name, Sum) -> vedic(Name, Sum + 8);
vedic("I"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("J"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("K"++Name, Sum) -> vedic(Name, Sum + 2);
vedic("L"++Name, Sum) -> vedic(Name, Sum + 3);
vedic("M"++Name, Sum) -> vedic(Name, Sum + 4);
vedic("N"++Name, Sum) -> vedic(Name, Sum + 5);
vedic("O"++Name, Sum) -> vedic(Name, Sum + 7);
vedic("P"++Name, Sum) -> vedic(Name, Sum + 8);
vedic("Q"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("R"++Name, Sum) -> vedic(Name, Sum + 2);
vedic("S"++Name, Sum) -> vedic(Name, Sum + 3);
vedic("T"++Name, Sum) -> vedic(Name, Sum + 4);
vedic("U"++Name, Sum) -> vedic(Name, Sum + 6);
vedic("V"++Name, Sum) -> vedic(Name, Sum + 6);
vedic("W"++Name, Sum) -> vedic(Name, Sum + 6);
vedic("X"++Name, Sum) -> vedic(Name, Sum + 6);
vedic("Y"++Name, Sum) -> vedic(Name, Sum + 1);
vedic("Z"++Name, Sum) -> vedic(Name, Sum + 7);
vedic([_|Name], Sum) -> vedic(Name, Sum + 0).

