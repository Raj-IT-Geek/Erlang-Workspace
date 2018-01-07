
-module(inch_to_cm).
-export([convert/1]).

convert(Inches) -> Inches * 2.54.
