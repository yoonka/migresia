-module(migresia).

-include_lib("eunit/include/eunit.hrl").

-export([check/0, migrate/0]).

-define(APP, migresia).
-define(TABLE, schema_migrations).

check() ->
    Loaded = get_migrations(),
    if Loaded == [] ->
            io:format("No migrations to apply, finishing~n", []);
       true ->
            io:format("Migrations to apply: ~p~n", [ [X||{X,_} <- Loaded] ])
    end.

migrate() ->
    Loaded = get_migrations(),
    lists:foreach(fun execute_up/1, Loaded).

get_migrations() ->
    application:load(?APP),
    get_migrations(application:get_env(?APP, dir)).

get_migrations(undefined) ->
    exit({undefined, migrate_dir});
get_migrations({ok, Dir}) ->
    ToApply = check_dir(file:list_dir(Dir)),
    start_mnesia(),
    Applied = check_table(),
%%    io:format("ToApply:~n~p~nApplied:~n~p~n", [ToApply, Applied]),
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
        ok ->
            io:format(" => started~nWaiting for tables...~n", []),
            ok = mnesia:wait_for_tables([?TABLE], 5000),
            io:format(" => done~n", []);
        Other ->
            io:format(" => Error:~p~nExiting...~n", [Other]),
            exit(Other)
    end.

check_table() ->
    case lists:member(?TABLE, mnesia:system_info(tables)) of
        false ->
            create_migration_table();
        true ->
            Select = [{{?TABLE,'_','_'},[],['$_']}],
            List = mnesia:dirty_select(?TABLE, Select),
            [ X || {schema_migrations, X, true} <- List ]
    end.

create_migration_table() ->
    io:format("Table schema_migration not found, creating...~n", []),
    case mnesia:create_table(?TABLE, [{type, ordered_set}, {disc_copies, list_nodes()}]) of
        {atomic, ok} ->
            io:format(" => created~n", []),
            [];
        {aborted, Reason} ->
            exit({mnesia, create_table, Reason})
    end.

list_nodes() ->
    Node = node(),
    if Node == nonode@nohost ->
            io:format("Warning: Erlant isn't run in distributed mode.~n", []);
       true ->
            ok
    end,
    [Node].

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
            io:format("Warnings: ~p~nErrors: ~p~nExiting...", [Warnings, Errors]),
            exit(Errors);
        error ->
            io:format("Unknown error encoutered, Exiting...", []),
            exit({compile, file, error})
    end.

load_migration(Module, Short, Binary) ->
    case code:load_binary(Module, Module, Binary) of
        {module, Module} ->
            {Module, Short};
        {error, What} ->
            exit({code, load_binary, What})
    end.

execute_up({Module, Short}) ->
    io:format("Executing up in ~s...~n", [Module]),
    Trans = fun() ->
                    Module:up(),
                    mnesia:write(schema_migrations, {schema_migrations, Short, true}, write)
            end,
    case mnesia:transaction(Trans) of
        {aborted, Reason} ->
            exit({mnesia, transaction, Reason});
        {atomic, _} ->
            io:format(" => done~n", [])
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
