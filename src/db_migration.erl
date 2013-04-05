-module(db_migration).

%% API

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{up, 0},
     {down, 0}];
behaviour_info(_Other) ->
    undefined.
