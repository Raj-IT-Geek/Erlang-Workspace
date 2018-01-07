-module(gettest).
-compile(export_all).

put(Hey) -> Hey ++ " put.".

get(Hey) -> Hey ++ " get.".
