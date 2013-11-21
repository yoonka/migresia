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

-export([create_new_migration/2, check/1, migrate/1, list_disc_copy_nodes/0]).

-define(TABLE, schema_migrations).

-spec create_new_migration(atom(), string()) -> any().
create_new_migration(App, Description) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    Filename = lists:flatten(io_lib:format("~w~2.2.0w~2.2.0w~2.2.0w~2.2.0w~2.2.0w_", [Year, Month, Day, Hour, Minute, Second]) ++ Description),
    Path = migresia_migrations:get_priv_dir(App),
    FullPathAndExtension = filename:join(Path, Filename ++ ".erl"),
    io:format("Creating new migration: ~p~n", [FullPathAndExtension]),
    ok = filelib:ensure_dir(<<Path/binary, <<"/">>/binary>>),
    ok = file:write_file(FullPathAndExtension, io_lib:fwrite("-module(~p).~n-behavior(db_migration).~n-export([up/0, down/0]). ~n~nup() -> ok.~n~ndown() -> throw(<<\"Downgraders not implemented.\">>).", [list_to_atom(Filename)])),
    io:format("Migration written.~n~n").

-spec check(atom()) -> ok | {error, any()}.
check(App) ->
    case start_mnesia(false) of
        ok -> 
            Loaded = migresia_migrations:list_unapplied_ups(App),
            if Loaded == [] ->
                    [];
               true ->
                    [ [X||{X,_} <- Loaded] ]
            end;
        {error, Error} -> {error, Error}
    end.

-spec migrate(atom()) -> ok | {error, any()}.
migrate(App) ->
    case start_mnesia(true) of
        ok ->
            case migresia_migrations:ensure_schema_table_exists() of
                ok ->
                    rpc:multicall(migresia_migrations, list_unapplied_ups, [App]), %Basically just to ensure everybody has loaded all the migrations, which is necessary in distributed Mnesia transforms.
                    Loaded = migresia_migrations:list_unapplied_ups(App),
                    lists:foreach(fun execute_up/1, Loaded);
                {error, Error} -> Error
            end;
        {error, Error} -> {error, Error}
    end.

execute_up({Module, Short}) ->
    io:format("Executing up in ~s...~n", [Module]),
    Module:up(),
    mnesia:dirty_write(schema_migrations, {schema_migrations, Short, true}),
    io:format(" => done~n", []).



-spec start_mnesia(boolean()) -> ok | {error, any()}.
start_mnesia(RemoteToo) ->
    io:format("Starting Mnesia...~n", []),
    case application:ensure_started(mnesia) of
        Other when Other /= ok -> io:format(" => Error:~p~n", [Other]), Other;
        ok -> 
            case RemoteToo of
                false -> ok;
                true -> {ResultList, BadNodes} = rpc:multicall(list_disc_copy_nodes(), application, ensure_started, [mnesia]),
                    BadStatuses = [X || X <- ResultList, X /= ok],
                    if BadNodes /= [] -> io:format(" => Error:~p~n", [not_all_nodes_running]), {error, not_all_nodes_running};
                        BadStatuses /= [] -> io:format(" => Error:~p~n", [BadStatuses]), {error, BadStatuses};
                        true -> io:format(" => started~n", []), ok
                    end
            end
    end.

list_disc_copy_nodes() -> 
    mnesia:table_info(schema, disc_copies).