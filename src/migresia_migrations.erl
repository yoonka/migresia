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

-module(migresia_migrations).

-include_lib("eunit/include/eunit.hrl").

-export([list_unapplied_ups/1, get_priv_dir/1]).

-define(APP, migresia).
-define(DIR, <<"migrate">>).
-define(TABLE, schema_migrations).

-spec list_unapplied_ups(atom()) -> [{module(), binary()}].
list_unapplied_ups(App) when App == undefined ->
    list_unapplied_ups(?APP);
list_unapplied_ups(App) ->
    get_migrations(get_priv_dir(App)).


get_priv_dir(App) when App == undefined ->
    get_priv_dir(?APP);
get_priv_dir(App) ->
    application:load(App),
    filename:join(code:priv_dir(App), ?DIR).

get_migrations({error, _} = Err) ->
    exit(Err);
get_migrations(Dir) ->
    io:format("DIR: ~p~n~n", [Dir]),
    ToApply = check_dir(file:list_dir(Dir)),
    start_mnesia(),
    Applied = check_table(),
    ToExecute = compile_unapplied(Dir, ToApply, Applied, []),
    Fun = fun({Module, Short, Binary}) -> load_migration(Module, Short, Binary) end,
    lists:map(Fun, ToExecute).

check_dir({error, Reason}) ->
    exit({file, list_dir, Reason});
check_dir({ok, Filenames}) ->
    normalize_names(Filenames, []).

normalize_names([<<Short:14/bytes, ".erl">>|T], Acc) ->
    normalize_names(T, [{Short, Short}|Acc]);
normalize_names([<<Short:14/bytes, $_, R/binary>> = Name|T], Acc)
  when size(R) >= 4 andalso erlang:binary_part(R, size(R) - 4, 4) == <<".erl">> ->
    Base = erlang:binary_part(Name, 0, size(Name) - 4),
    normalize_names(T, [{Short, Base}|Acc]);
normalize_names([Name|T], Acc) when is_list(Name) ->
    normalize_names([list_to_binary(Name)|T], Acc);
normalize_names([Name|_], _Acc) ->
    exit({badmatch, Name});
normalize_names([], Acc) ->
    lists:sort(Acc).

start_mnesia() ->
    io:format("Starting Mnesia...~n", []),
    case mnesia:start() of
        ok    -> io:format(" => started~n", []);
        Other -> io:format(" => Error:~p~nExiting...~n", [Other]), exit(Other)
    end.

check_table() ->
    case lists:member(?TABLE, mnesia:system_info(tables)) of
        false ->
            create_migration_table();
        true ->
            io:format("Waiting for tables...~n", []),
            ok = mnesia:wait_for_tables([?TABLE], 5000),
            io:format(" => done~n", []),
            Select = [{{?TABLE,'_','_'},[],['$_']}],
            List = mnesia:dirty_select(?TABLE, Select),
            [ X || {schema_migrations, X, true} <- List ]
    end.

create_migration_table() ->
    io:format("Table schema_migration not found, creating...~n", []),
    Attr = [{type, ordered_set}, {disc_copies, migresia:list_nodes()}],
    case mnesia:create_table(?TABLE, Attr) of
        {atomic, ok}      -> io:format(" => created~n", []), [];
        {aborted, Reason} -> exit({mnesia, create_table, Reason})
    end.

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
        {error, Errors, Warnings} ->
            io:format("Warnings: ~p~nErrors: ~p~nExiting...~n", [Warnings, Errors]),
            exit(Errors);
        error ->
            io:format("Unknown error encoutered, Exiting...~n", []),
            exit({compile, file, error})
    end.

load_migration(Module, Short, Binary) ->
    case code:load_binary(Module, Module, Binary) of
        {module, Module} ->
            {Module, Short};
        {error, What} ->
            exit({code, load_binary, What})
    end.

%%% Some unit tests

normalize_test() ->
    [{<<"20130401120000">>, <<"20130401120000">>},
     {<<"20130402000000">>, <<"20130402000000_">>},
     {<<"20130403080910">>, <<"20130403080910_test">>}] =
        normalize_names(
          [<<"20130402000000_.erl">>,
           "20130403080910_test.erl",
           <<"20130401120000.erl">>], []).
