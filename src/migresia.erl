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
         ensure_started/1,
         check/1,
         migrate/1,
         rollback/2,
         list_nodes/0]).

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

-spec migrate(atom()) -> ok | {error, any()}.
migrate(App) ->
    migrate(migresia_migrations:init_migrations(), App).

migrate(ok, App) ->
    io:format("Waiting for tables (max timeout 2 minutes)...", []),
    ok = mnesia:wait_for_tables(mnesia:system_info(tables), 120000),
    Loaded = migresia_migrations:list_unapplied_ups(App),
    %% Load the transform function on all nodes, see:
    %% http://toddhalfpenny.com/2012/05/21/possible-erlang-bad-transform-function-solution/
    rpc:multicall(nodes(), migresia_migrations, list_unapplied_ups, [App]),
    lists:foreach(fun migresia_migrations:execute_up/1, Loaded);
migrate({error, _} = Err, _) ->
    Err.

%%------------------------------------------------------------------------------

-spec rollback(atom(), integer()) -> ok | {error, any()}.
rollback(App, Time) ->
    rollback(migresia_migrations:init_migrations(), App, Time).

rollback(ok, App, Time) ->
    io:format("Waiting for tables (max timeout 2 minutes)...", []),
    ok = mnesia:wait_for_tables(mnesia:system_info(tables), 120000),
    Ups = migresia_migrations:list_all_ups(App),
    %% Load the transform function on all nodes, see:
    %% http://toddhalfpenny.com/2012/05/21/possible-erlang-bad-transform-function-solution/
    rpc:multicall(nodes(), migresia_migrations, list_all_ups, [App]),
    ToRollBack = lists:reverse([X || {_, Ts, _} = X <- Ups, Ts > Time]),
    lists:foreach(fun migresia_migrations:execute_down/1, ToRollBack);
rollback({error, _} = Err, _App, _Time) ->
    Err.
