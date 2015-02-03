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
         get_priv_dir/1,
         execute_up/1,
         execute_down/1]).

-define(DIR, <<"migrate">>).
-define(TABLE, schema_migrations).

-type error() :: {error, any()}.
-type mod_bin_list() :: [{module(), binary()}].

%%------------------------------------------------------------------------------

-spec init_migrations() -> ok | error().
init_migrations() ->
    case lists:member(?TABLE, mnesia:system_info(tables)) of
        true ->
            ok;
        false ->
            io:format("Table schema_migration not found, creating...~n", []),
            Attr = [{type, ordered_set}, {disc_copies, migresia:list_nodes()}],
            case mnesia:create_table(?TABLE, Attr) of
                {atomic, ok}      -> io:format(" => created~n", []), ok;
                {aborted, Reason} -> {error, Reason}
            end
    end.

%%------------------------------------------------------------------------------

-spec list_unapplied_ups(atom()) -> mod_bin_list() | error().
list_unapplied_ups(App) ->
    get_migrations(get_priv_dir(App)).

-spec list_all_ups(atom()) -> mod_bin_list() | error().
list_all_ups(App) ->
    get_all_migrations(get_priv_dir(App)).

-spec get_priv_dir(atom()) -> string() | binary() | error().
get_priv_dir(App) ->
    case application:load(App) of
        ok ->
            filename:join(code:priv_dir(App), ?DIR);
        {error, {already_loaded, App}} ->
            filename:join(code:priv_dir(App), ?DIR);
        {error, _} = Err ->
            Err
    end.

-spec get_migrations({error, any()} | binary()) -> mod_bin_list() | error().
get_migrations({error, _} = Err) ->
    Err;
get_migrations(Dir) ->
    ToApply = check_dir(file:list_dir(Dir)),
    case check_table() of
        {error, _} = Err -> Err;
        Applied -> compile_and_load(Dir, ToApply, Applied)
    end.

get_all_migrations(Dir) ->
    ToApply = lists:sort(check_dir(file:list_dir(Dir))),
    compile_and_load(Dir, ToApply, []).

check_dir({error, Reason}) ->
    throw({file, list_dir, Reason});
check_dir({ok, Filenames}) ->
    normalize_names(Filenames, []).

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
                {error, _} = Err ->
                    Err;
                Error ->
                    {error, Error}
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
            io:format("Warnings: ~p~nErrors: ~p~nAborting...~n", [Warn, Err]),
            throw({compile, compile_error, Err});
        error ->
            io:format("Unknown error encoutered, Aborting...~n", []),
            throw({compile, unknown_error, File})
    end.

load_migration(Module, Short, Binary) ->
    case code:load_binary(Module, Module, Binary) of
        {module, Module} -> {Module, Short};
        {error, What} -> throw({code, load_binary, What})
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
