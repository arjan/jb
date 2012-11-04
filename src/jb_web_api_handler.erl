-module(jb_web_api_handler).

-include_lib("espotify/include/espotify.hrl").

-export([init/3,
         handle/2,
         terminate/2,
         unknown_method/2,
         method_next/2,
         method_stop/2,
         method_toggle_pause/2,
         method_queue/2,
         method_queue_add/2,
         method_status/2,
         method_seek/2
        ]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:binding(method, Req),
    Fun = try
              list_to_existing_atom("method_" ++ binary_to_list(Method))
          catch
              error:badarg ->
                  unknown_method
          end,
    Headers = [{"Content-type", "application/json"}],
    case ?MODULE:Fun(Req, State) of
        {ok, JSON} ->
            {ok, Req3} = cowboy_req:reply(200, Headers, jiffy:encode({[{reply, JSON}]}), Req2),
            {ok, Req3, State};
        {error, Reason} ->
            {ok, Req3} = cowboy_req:reply(500, Headers, jiffy:encode({[{error, Reason}]}), Req2),
            {ok, Req3, State}
    end.
            

terminate(_Req, _State) ->
    ok.

unknown_method(_, _) ->
    {error, unknown_method}.


method_next(_, _) ->
    ok = jb_player:next(),
    {ok, jb_web:json_status()}.

method_stop(_, _) ->
    ok = jb_player:stop(),
    {ok, jb_web:json_status()}.

method_toggle_pause(_, _) ->
    ok = jb_player:toggle_pause(),
    {ok, jb_web:json_status()}.

method_status(_, _) ->
    {ok, jb_web:json_status()}.

method_queue(_, _) ->
    {ok, Q} = jb_queue:get_queue(),
    {ok, [jb_web:record_to_json(sp_track, T) || T <- Q]}.

method_seek(Req, _) ->
    {T, _R} = cowboy_req:qs_val(<<"t">>, Req, <<>>),
    Pos = list_to_integer(binary_to_list(T)),
    jb_player:seek(Pos),
    {ok, jb_web:json_status()}.
    
method_queue_add(Req, _) ->
    {L, _R} = cowboy_req:qs_val(<<"link">>, Req, <<>>),
    case binary_to_list(L) of
        "spotify:" ++ _ = Link ->
            jb_queue:queue(Link),
            {ok, ok};
        _ ->
            {error, invalid_link_argument}
    end.
