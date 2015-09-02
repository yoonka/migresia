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

-module(migresia_migrations).

-export([init_migrations/0,
         list_unapplied_ups/1,
         list_all_ups/1,
         get_default_dir/0,
         get_priv_dir/1,
         execute_up/1,
         execute_down/1]).

-define(DIR, <<"migrations">>).
-define(TABLE, schema_migrations).

-type mod_bin_list() :: [{module(), binary()}].

%%------------------------------------------------------------------------------

-spec init_migrations() -> ok.
init_migrations() ->
    case lists:member(?TABLE, mnesia:system_info(tables)) of
        true ->
            ok;
        false ->
            io:format("Table schema_migration not found, creating...~n", []),
            Attr = [{type, ordered_set}, {disc_copies, migresia:list_nodes()}],
            case mnesia:create_table(?TABLE, Attr) of
                {atomic, ok}      -> io:format(" => created~n", []);
                {aborted, Reason} -> throw({error, Reason})
            end
    end.

%%------------------------------------------------------------------------------

-spec list_unapplied_ups(migresia:migration_sources())
                        -> mod_bin_list().
list_unapplied_ups({rel_relative_dir, default}) ->
    list_unapplied_ups({rel_relative_dir, get_default_dir()});
list_unapplied_ups({rel_relative_dir, DirName}) ->
    get_migrations(get_release_dir(DirName));
list_unapplied_ups(App) when is_atom(App) ->
    get_migrations(get_priv_dir(App)).

-spec list_all_ups(migresia:migration_sources()) -> mod_bin_list().
list_all_ups({rel_relative_dir, default}) ->
    list_all_ups({rel_relative_dir, get_default_dir()});
list_all_ups({rel_relative_dir, DirName}) ->
    get_all_migrations(get_release_dir(DirName));
list_all_ups(App) when is_atom(App) ->
    get_all_migrations(get_priv_dir(App)).

get_default_dir() ->
    case application:get_env(migresia, rel_relative_dir) of
        {ok, Val} -> Val;
        undefined -> ?DIR
    end.

get_release_dir(DirName) ->
    case filelib:is_dir(DirName) of
        true -> DirName;
        false -> try_to_cwd(DirName)
    end.

try_to_cwd(DirName) ->
    Root = code:root_dir(),
    case filelib:is_dir(filename:join(Root, code:lib_dir(migresia))) of
        true ->
            file:set_cwd(Root),
            case filelib:is_dir(DirName) of
                true -> DirName;
                false -> throw({error, enoent})
            end;
        false -> throw({error, badcwd})
    end.

-spec get_priv_dir(atom()) -> string() | binary().
get_priv_dir(App) ->
    case application:load(App) of
        ok -> check_priv_dir(App);
        {error, {already_loaded, App}} -> check_priv_dir(App);
        {error, _} = Err -> throw(Err)
    end.

check_priv_dir(App) ->
    Dir = filename:join(code:priv_dir(App), ?DIR),
    case filelib:is_dir(Dir) of
        true -> Dir;
        false -> throw({error, enoent})
    end.

-spec get_migrations(binary()) -> mod_bin_list().
get_migrations(Dir) ->
    ToApply = check_dir(file:list_dir(Dir)),
    case check_table() of
        {error, _} = Err -> throw(Err);
        Applied -> compile_and_load(Dir, ToApply, Applied)
    end.

get_all_migrations(Dir) ->
    ToApply = lists:sort(check_dir(file:list_dir(Dir))),
    compile_and_load(Dir, ToApply, []).

check_dir({error, _} = Err) -> throw(Err);
check_dir({ok, Filenames}) -> normalize_names(Filenames, []).

normalize_names([<<Short:14/bytes, ".erl">>|T], Acc) ->
    normalize_names(T, [{Short, Short}|Acc]);
normalize_names([<<Short:14/bytes, $_, R/binary>> = Name|T], Acc)
  when size(R) >= 4
       andalso erlang:binary_part(R, size(R) - 4, 4) == <<".erl">> ->
    Base = erlang:binary_part(Name, 0, size(Name) - 4),
    Int = list_to_integer(binary_to_list(Short)),
    normalize_names(T, [{Int, Base}|Acc]);
normalize_names([Name|T], Acc) when is_list(Name) ->
    normalize_names([list_to_binary(Name)|T], Acc);
normalize_names([Name|T], Acc) ->
    io:format("Ignoring: ~p~n", [Name]),
    normalize_names(T, Acc);
normalize_names([], Acc) ->
    lists:sort(Acc).

check_table() ->
    case lists:member(?TABLE, mnesia:system_info(tables)) of
        false ->
            [];
        true ->
            io:format("Waiting for tables...~n", []),
            case mnesia:wait_for_tables([?TABLE], 5000) of
                ok ->
                    io:format(" => done~n", []),
                    Select = [{{?TABLE,'_','_'},[],['$_']}],
                    List = mnesia:dirty_select(?TABLE, Select),
                    [ X || {?TABLE, X, true} <- List ];
                {error, _} = Err -> throw(Err);
                Error -> throw({error, Error})
            end
    end.

compile_and_load(Dir, ToApply, Applied) ->
    ToExecute = compile_unapplied(Dir, ToApply, Applied, []),
    Fun = fun({Module, Short, Binary}) ->
                  load_migration(Module, Short, Binary)
          end,
    lists:map(Fun, ToExecute).

%%------------------------------------------------------------------------------

compile_unapplied(Dir, [{Short, Module}|TN], [] = Old, Acc) ->
    compile_unapplied(Dir, TN, Old, [compile_file(Dir, Short, Module)|Acc]);
compile_unapplied(Dir, [{Last, _}|TN], [Last], Acc) ->
    compile_unapplied(Dir, TN, [], Acc);
compile_unapplied(Dir, [{Last, _}], [Last|_TO], Acc) ->
    compile_unapplied(Dir, [], [], Acc);
compile_unapplied(Dir, [{Short, _}|TN], [Short|TO], Acc) ->
    compile_unapplied(Dir, TN, TO, Acc);
compile_unapplied(Dir, [{New, _}|TN], [Old|_] = Applied, Acc) when New < Old ->
    compile_unapplied(Dir, TN, Applied, Acc);
compile_unapplied(Dir, [{New, _}|_] = ToApply, [Old|TO], Acc) when New > Old ->
    compile_unapplied(Dir, ToApply, TO, Acc);
compile_unapplied(_Dir, [], _, Acc) ->
    lists:reverse(Acc).

compile_file(Dir, Short, Name) ->
    File = filename:join(Dir, Name),
    io:format("Compiling: ~s~n", [File]),
    case compile:file(binary_to_list(File), [verbose, binary, report]) of
        {ok, Module, Binary} ->
            {Module, Short, Binary};
        {ok, Module, Binary, Warnings} ->
            io:format("Warnings: ~p~n", [Warnings]),
            {Module, Short, Binary};
        {error, Err, Warn} ->
            io:format("Errors: ~p~nWarnings: ~p~nAborting...~n", [Err, Warn]),
            throw({error, compile_error});
        error ->
            io:format("Errors encoutered, Aborting...~n", []),
            throw({error, compile_error})
    end.

load_migration(Module, Short, Binary) ->
    case code:load_binary(Module, Module, Binary) of
        {module, Module} ->
            {Module, Short};
        {error, _} = Err ->
            io:format("Error when loading module '~p'.~n", [Module]),
            throw(Err)
    end.

%%------------------------------------------------------------------------------

execute_up({Module, Ts}) ->
    io:format("Executing up in ~s...~n", [Module]),
    Module:up(),
    mnesia:dirty_write(?TABLE, {?TABLE, Ts, true}),
    io:format(" => done~n", []).

execute_down({Module, Ts}) ->
    io:format("Executing down in ~s...~n", [Module]),
    Module:down(),
    mnesia:dirty_delete(?TABLE, Ts),
    io:format(" => done~n", []).
