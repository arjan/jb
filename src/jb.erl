%% Application helper

-module(jb).

-export([start/0]).

start() ->
    start(jb).

start(App) ->
    case application:start(App) of
        {error, {not_started, Dep}} ->
            ok = start(Dep),
            ok = start(App);
        Other ->
            Other
    end.
    
