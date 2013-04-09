-module(migresia).

-export([check/0, migrate/0, list_nodes/0]).

check() ->
    Loaded = migresia_migrations:list_unapplied_ups(),
    if Loaded == [] ->
            io:format("No migrations to apply.~n", []);
       true ->
            io:format("Migrations to apply: ~p~n", [ [X||{X,_} <- Loaded] ])
    end.

migrate() ->
    Loaded = migresia_migrations:list_unapplied_ups(),
    lists:foreach(fun execute_up/1, Loaded).

execute_up({Module, Short}) ->
    io:format("Executing up in ~s...~n", [Module]),
    Module:up(),
    mnesia:dirty_write(schema_migrations, {schema_migrations, Short, true}),
    io:format(" => done~n", []).

list_nodes() ->
    Node = node(),
    if Node == nonode@nohost ->
            io:format("Warning: Erlang doesn't seem to be running in distributed mode.~n", []);
       true ->
            ok
    end,
    [Node].
