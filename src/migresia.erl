%% Copyright (c) 2013, Grzegorz Junka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% Redistributions of source code must retain the above copyright notice,
%% this list of conditions and the following disclaimer.
%% Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(migresia).

-export([create_new_migration/1, create_new_migration/2, check/0, check/1, migrate/0, migrate/1, list_nodes/0]).

-spec create_new_migration(string()) -> any().
create_new_migration(Description) ->
    create_new_migration(undefined, Description).

-spec create_new_migration(atom(), string()) -> any().
create_new_migration(App, Description) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    Filename = lists:flatten(io_lib:format("~w~2.2.0w~2.2.0w~2.2.0w~2.2.0w~2.2.0w_", [Year, Month, Day, Hour, Minute, Second]) ++ Description),
    FullPathAndExtension = filename:join(migresia_migrations:get_priv_dir(App), Filename ++ ".erl"),
    io:format("Creating new migration: ~p~n", [FullPathAndExtension]),
    file:write_file(FullPathAndExtension, io_lib:fwrite("-module(~p).~n-behavior(db_migration).~n-export([up/0, down/0]). ~n~nup() -> ok.~n~ndown() -> throw(<<\"Downgraders not implemented.\">>)", [list_to_atom(Filename)])),
    io:format("Migration written.~n~n").

-spec check(atom()) -> any().
check(App) ->
    Loaded = migresia_migrations:list_unapplied_ups(App),
    if Loaded == [] ->
            io:format("No migrations to apply.~n", []);
       true ->
            io:format("Migrations to apply: ~p~n", [ [X||{X,_} <- Loaded] ])
    end.

-spec check() -> any().
check() -> check(undefined).

-spec migrate(atom()) -> any().
migrate(App) ->
    Loaded = migresia_migrations:list_unapplied_ups(App),
    lists:foreach(fun execute_up/1, Loaded).

-spec migrate() -> any().
migrate() ->
    migrate(undefined).

execute_up({Module, Short}) ->
    io:format("Executing up in ~s...~n", [Module]),
    Module:up(),
    mnesia:dirty_write(schema_migrations, {schema_migrations, Short, true}),
    io:format(" => done~n", []).

list_nodes() ->
    mnesia:table_info(schema, disc_copies).

