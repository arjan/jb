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
         method_status/2
        ]).

init({tcp, http}, Req, Opts) ->
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
            

terminate(Req, State) ->
    ok.

unknown_method(_, _) ->
    {error, unknown_method}.


method_next(_, _) ->
    ok = jb_player:next(),
    {ok, json_status()}.

method_stop(_, _) ->
    ok = jb_player:stop(),
    {ok, json_status()}.

method_toggle_pause(_, _) ->
    ok = jb_player:toggle_pause(),
    {ok, json_status()}.

method_status(_, _) ->
    {ok, json_status()}.

method_queue(_, _) ->
    {ok, Q} = jb_queue:get_queue(),
    {ok, [record_to_json(sp_track, T) || T <- Q]}.

method_queue_add(Req, _) ->
    {L, _R} = cowboy_req:qs_val(<<"link">>, Req, <<>>),
    case binary_to_list(L) of
        "spotify:track:" ++ _ = Link ->
            jb_queue:queue(Link),
            {ok, ok};
        _ ->
            {error, invalid_link_argument}
    end.

json_status() ->
    {State, Position, Track0} = jb_player:status(),
    Track = case Track0 of
                undefined ->
                    null;
                #sp_track{} ->
                    record_to_json(sp_track, Track0)
            end,
    {[{status, State},
      {pos, Position},
      {track, Track}]}.


record_to_json(RecordName, Record) ->
  {lists:zip(
     lists:map(fun(F) ->
                       list_to_binary(atom_to_list(F))
               end,
               record_fields(RecordName)),
     lists:map(fun json_value/1,
               tl(tuple_to_list(Record)))
    )
  }.

json_value(undefined) -> null;
json_value(T) when is_tuple(T), is_atom(element(1, T)) -> 
    record_to_json(hd(tuple_to_list(T)), T);
json_value([Ch|_]=Str) when is_integer(Ch) ->
    list_to_binary(Str);
json_value(L) when is_list(L) ->
    lists:map(fun json_value/1, L);
json_value(E) -> E.

record_fields(sp_artist) -> record_info(fields, sp_artist);
record_fields(sp_album) -> record_info(fields, sp_album);
record_fields(sp_track) -> record_info(fields, sp_track).
     
