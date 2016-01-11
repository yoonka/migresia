-module('20160101112233_empty').
-behavior(db_migration).
-export([up/0, down/0]).

up() ->
    io:format("Empty migration applied.~n", []).

down() ->
    io:format("Empty migration reverted.~n", []).
