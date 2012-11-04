-module(jb_web).

-include_lib("espotify/include/espotify.hrl").

-export([
         setup/0,

         stream_event/2,
         
         stream_status/1,
         json_status/0,
         json_status/1,

         stream_queue/1,
         json_queue/0,
         json_queue/1,
         record_to_json/2
        ]).

setup() ->
	Dispatch =
        [
         {'_', [
                {[<<"api">>, method], jb_web_api_handler, []},
                {[<<"socket">>], jb_web_stream_handler, []},
                {[], cowboy_static,
                 [
                  {directory, {priv_dir, jb, []}},
                  {file, "index.html"},
                  {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                 ]},
                {['...'], cowboy_static,
                 [
                  {directory, {priv_dir, jb, []}},
                  {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                 ]} 
               ]}
        ],
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{dispatch, Dispatch}]).

stream_event(Event, JSON) ->
    {[{event, Event}, {data, JSON}]}.

stream_all(Event, JSON) ->
    Payload = stream_event(Event, JSON),
    Key = {jb_web_stream_handler, client},
    gproc:send({p, l, Key}, {stream, Payload}).

stream_status(Status) ->
    stream_all(status, json_status(Status)).

json_status() ->
    json_status(jb_player:status()).

json_status(Status) ->
    {State, Position, Track0} = Status,
    Track = case Track0 of
                undefined ->
                    null;
                #sp_track{} ->
                    record_to_json(sp_track, Track0)
            end,
    {[{status, State},
      {pos, Position},
      {track, Track}]}.



stream_queue(Queue) ->
    stream_all(queue, json_queue(Queue)).

json_queue() ->
    {ok, Q} = jb_queue:get_queue(),
    json_queue(Q).

json_queue(Queue) ->
    [record_to_json(sp_track, Track) || Track <- Queue].


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
     
