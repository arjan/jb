-module(jb_web_stream_handler).

-export([
         init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Atom, Req, _Opts) ->
    gproc:reg({p, l, {?MODULE, client}}),
    %% Send initial play state
    self() ! {stream, jb_web:stream_event(status, jb_web:json_status())},
    %% Send the current play queue
    self() ! {stream, jb_web:stream_event(queue, jb_web:json_queue())},
    {ok, Req, undefined_state}.

websocket_handle({text, <<"status">>}, Req, State) ->
    {reply, {text, jiffy:encode(jb_web:stream_event(status, jb_web:json_status()))}, Req, State};

websocket_handle(Data, Req, State) ->
    lager:warning("Data: ~p", [Data]),
    {ok, Req, State}.

websocket_info({stream, JSON}, Req, State) ->
    {reply, {text, jiffy:encode(JSON)}, Req, State};

websocket_info(Msg, Req, State) ->
    lager:warning("Msg: ~p", [Msg]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
