%% Copyright (c) 2015, Grzegorz Junka
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

-export([start_all_mnesia/0,
         list_nodes/0,
         list_migrations/0,
         ensure_started/1,
         check/1,
         migrate/0,
         migrate/1,
         rollback/1,
         rollback/2,
         rollback_last/0,
         rollback_last/1]).

%%------------------------------------------------------------------------------

-spec start_all_mnesia() -> ok | {error, any()}.
start_all_mnesia() ->
    io:format("Starting Mnesia...~n", []),
    case ensure_started(mnesia) of
        ok ->
            ensure_started_on_remotes(list_nodes());
        Err ->
            io:format(" => Error:~p~n", [Err]),
            Err
    end.

list_nodes() -> 
    mnesia:table_info(schema, disc_copies).

list_migrations() ->
    migresia_migrations:list_migrations().

ensure_started_on_remotes(Nodes) ->
    io:format("Ensuring Mnesia is running on nodes:~n~p~n", [Nodes]),
    {ResL, BadNodes} = rpc:multicall(Nodes, migresia, ensure_started, [mnesia]),
    handle_err([X || X <- ResL, X /= ok], BadNodes).

handle_err([], []) ->
    io:format(" => started~n", []),
    ok;
handle_err(Results, Bad) ->
    if Results /= [] -> io:format(" => Error, received: ~p~n", [Results]) end,
    if Bad /= [] -> io:format(" => Error, bad nodes: ~p~n", [Bad]) end,
    {error, mnesia_not_started}.

-spec ensure_started(atom()) -> ok | {error, any()}. 
ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok;
        {error, _} = Err -> Err
    end.

%%------------------------------------------------------------------------------

-spec check(atom()) -> ok | {error, any()}.
check(App) ->
    case migresia_migrations:list_unapplied_ups(App) of
        [] -> [];
        {error, _} = Err -> Err;
        Loaded -> [X || {X, _} <- Loaded]
    end.

%%------------------------------------------------------------------------------

-type migration_dir() :: default | file:filename().
-type migration_source() :: atom() | {rel_relative_dir, migration_dir()}.
-type migration_sources() :: migration_source(). %% | [migration_source()].

-spec migrate() -> ok | {error, any()}.
migrate() ->
    migrate({rel_relative_dir, default}).

-spec migrate(migration_sources()) -> ok | {error, any()}.
migrate(Srcs) ->
    try
        ok = migresia_migrations:init_migrations(),
        migrate1(Srcs)
    catch
        throw:{error, _} = Err -> Err
    end.

migrate1(Srcs) ->
    io:format("Waiting for tables (max timeout 2 minutes)...~n", []),
    ok = mnesia:wait_for_tables(mnesia:system_info(tables), 120000),
    case migresia_migrations:list_unapplied_ups(Srcs) of
        {error, _} = Err -> Err;
        Loaded -> apply_ups(Srcs, Loaded)
    end.

apply_ups(Srcs, Loaded) ->
    %% Load the transform function on all nodes, see:
    %% http://toddhalfpenny.com/2012/05/21/possible-erlang-bad-transform-function-solution/
    rpc:multicall(nodes(), migresia_migrations, list_unapplied_ups, [Srcs]),
    lists:foreach(fun migresia_migrations:execute_up/1, Loaded).

%%------------------------------------------------------------------------------

-spec rollback(integer()) -> ok | {error, any()}.
rollback(Time) ->
    rollback({rel_relative_dir, default}, Time).

-spec rollback(migration_sources(), integer()) -> ok | {error, any()}.
rollback(Srcs, Time) ->
    try
        ok = migresia_migrations:init_migrations(),
        rollback1(Srcs, Time)
    catch
        throw:{error, _} = Err -> Err
    end.

rollback1(Srcs, Time) ->
    io:format("Waiting for tables (max timeout 2 minutes)...~n", []),
    ok = mnesia:wait_for_tables(mnesia:system_info(tables), 120000),
    case migresia_migrations:list_applied_ups(Srcs, Time) of
        {error, _} = Err -> Err;
        Ups -> apply_downs(Srcs, Ups, Time)
    end.

apply_downs(Srcs, Loaded, Time) ->
    %% Load the transform function on all nodes, see:
    %% http://toddhalfpenny.com/2012/05/21/possible-erlang-bad-transform-function-solution/
    rpc:multicall(nodes(), migresia_migrations, list_applied_ups, [Srcs, Time]),
    lists:foreach(fun migresia_migrations:execute_down/1, Loaded).

%%------------------------------------------------------------------------------

-spec rollback_last() -> ok | {error, any()}.
rollback_last() -> rollback(migresia_migrations:get_ts_before_last()).

-spec rollback_last(migration_sources()) -> ok | {error, any()}.
rollback_last(Srcs) -> rollback(Srcs, migresia_migrations:get_ts_before_last()).
