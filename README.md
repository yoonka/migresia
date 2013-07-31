Migresia
========

A simple Erlang tool to automatically migrate Mnesia databases between versions. It's loosely based on Active Record Migrations from Ruby on Rails.

## How it works

When migrating the database:

1. List all available migrations.
2. Check which migrations haven't yet been applied.
3. Compile all unapplied migrations.
4. If the compilation went OK, load all compiled migrations.
5. For each loaded migration:

* Execute the `up` / `down`\* function as required to bring the database to the desired version.
* Mark the migration as applied if it has been executed successfully.

\* - migrating the database backward hasn't yet been implemented.

Migresia stores all applied migrations in table `schema_migrations` which it creates in case it doesn't yet exist.

## Migrations

Each migration is an Erlang source `*.erl` file stored in folder `priv/migrate/`, where `priv` is the private folder of the Migresia application. That folder is ignored in the Migresia repository and it is recommended that `priv/migrate` is a link to a folder within the main project, of which Migresia is a dependency, where it should be kept under a version control.

Migresia compiles the migrations automatically when it applies them, so they should be always distributed as source files. This is mainly to allow keeping applied migrations under a version control but not have to compile them every time when building the application or creating a release.

When developing a new migration just use `migresia:check/0` to let Migresia to compile them and report any problems.

#### Migration names

The name of each migration starts with a timestamp, which is treated as its version, and ends with a short information of the purpose of that migration. For example:

    20130731163300_convert_permissions.erl

The timestamp is always 14 numbers, but otherwise can contain any value. Migresia doesn't interpret it in any way, but it uses it to sort the migrations according to the value of that timestamp, assuming timestamps with the lower values are younger.

The remaining part of the name, after the timestamp, is simply ignored. A migration name consisting of just the timestamp is also a valid name.

#### Implementing migrations

Migresia doesn't provide any special support for transactions. If any operations should be executed within a transaction, it just should be created and handled accordingly in the `up` or `down` functions.

Migresia provides a behaviour which all migrations should implement: `db_migration`. It expects two functions: `up/0` and `down/0`. Please note, that at this moment migrations can't be applied backward, so the `down/0` function is unused. However, both functions are present for completeness as it is expected that in the future Migresia will support migrating databases in both directions.

Very often migrations need to know the record definitions of Mnesia tables to which the migrations will be applied. The preferred way of doing this is by creating in the Migresia `include` folder a symbolic link to the include file that contains the required record definition. By default the Migresia repository ignores all include files in the Migresia `include` directory. The linked file should then be included using `include_lib`, for example:

    -module('20130731163300_convert_permissions').
    -behaviour(db_migration).
    
    -export([up/0, down/0]).
    
    -include_lib("migresia/include/tables.hrl").
    
    up() ->
        Permissions = #permissions{superuser = true},
        mnesia:dirty_write(#user_account{name = <<"root">>, value = Permissions}).

    down() ->
        throw(<<"Irreversible migration">>).

## API Calls

Migresia exports three simple functions:

##### `migresia:check/0`

This function does three things:

* Create the `schema_migrations` table if it doesn't yet exist.
* List available migrations and check which ones haven't yet been applied.
* Compile and load all unapplied migrations.

This is the method that should be used to check migrations for compilation errors as well as to verify which migrations will be applied if `migresia:migrate/0` is executed to migrate the database.

##### `migresia:migrate/0`

Works almost exactly like `migresia:check/0` but in the end, instead of printing information which migrations are going to be applied, just applies them by executing the required `up` or `down` functions.

##### `migresia:list_nodes/0`

Returns the output of `mnesia:table_info(schema, disc_copies)` which could be used in migrations to migrate databases on multiple nodes.
