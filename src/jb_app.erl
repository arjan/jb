-module(jb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    jb_web:setup(),
    jb_sup:start_link().

stop(_State) ->
    ok.
