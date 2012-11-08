%% @doc Player FSM.
%%

-module(jb_player).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include_lib("espotify/include/espotify.hrl").

-record(state,
        {current_track = undefined :: undefined | #sp_track{},
         spotify_user = undefined :: undefined | #sp_user{},
         playlist_container = undefined  :: undefined | #sp_playlistcontainer{},
         playhead_offset = 0,
         playhead_lastupdate = undefined
        }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/0,
         next/0,
         stop/0,
         toggle_pause/0,
         status/0,
         seek/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).
-export([
         unauthorized/2
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

next() ->
    gen_fsm:send_all_state_event(?SERVER, next).

stop() ->
    gen_fsm:send_all_state_event(?SERVER, stop).

status() ->
    gen_fsm:sync_send_all_state_event(?SERVER, status).
    
toggle_pause() ->
    gen_fsm:send_all_state_event(?SERVER, toggle_pause).

seek(T) ->
    gen_fsm:send_all_state_event(?SERVER, {seek, T}).
    


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, unauthorized, #state{}, 0}.

%% @doc Initial login
unauthorized(timeout, State) ->
    {next_state, unauthorized, do_try_login(State)}.

handle_event(next, _StateName, State) ->
    do_handle_next(State);

handle_event(toggle_pause, playing, State) ->
    espotify_api:player_play(false),
    State1 = State#state{playhead_offset=playhead_position(playing, State)},
    jb_web:stream_status(get_status(paused, State1)),
    {next_state, paused, State1};

handle_event(toggle_pause, paused, State) ->
    espotify_api:player_play(true),
    State1 = State#state{playhead_lastupdate=now_msec()},
    jb_web:stream_status(get_status(playing, State1)),
    {next_state, playing, State1};

handle_event(stop, S, State) when S =:= playing; S =:= paused ->
    set_state_stopped(State);

handle_event({seek, T}, S, State) when S =:= playing; S =:= paused ->
    espotify_api:player_seek(T),
    State1 = State#state{playhead_offset=T, playhead_lastupdate=now_msec()},
    jb_web:stream_status(get_status(S, State1)),
    {next_state, S, State1};

handle_event(_Event, _StateName, State) ->
    lager:warning("<~p> Unhandled event: ~p", [_StateName, _Event]),
    {next_state, _StateName, State}.

handle_sync_event(status, _From, StateName, State) ->
    {reply, get_status(StateName, State), StateName, State};

handle_sync_event(_Event, _From, _StateName, State) ->
    lager:warning("Unhandled sync event: ~p", [_Event]),
    {next_state, _StateName, State}.

handle_info({'$spotify_callback', logged_in, R}, _StateName, State) ->
    case R of
        {ok, User} ->
            lager:warning("User: ~p", [User]),
            State1 = State#state{spotify_user=User},
            do_handle_next(State1);
        {error, E} ->
            lager:error("Error: ~p", [E]),
            espotify_api:stop(),
            {next_state, unauthorized, State, 60000}
    end;
             
handle_info({'$spotify_callback', load_playlistcontainer, {ok, {undefined, PC}}}, StateName, State) ->
    {next_state, StateName, State#state{playlist_container=PC}};

handle_info({'$spotify_callback', player_load, {ok, Track}}, loading, State) ->
    set_state_playing(Track, State);

handle_info({'$spotify_callback', player_play, end_of_track}, playing, State) ->
    do_handle_next(State);

handle_info({'$spotify_callback', track_info, {ok, {_Ref, Track}}}, StateName, State) ->
    jb_queue:track_loaded(Track),
    {next_state, StateName, State};

handle_info({'$spotify_callback', browse_album, {ok, {_Ref, AlbumBrowse}}}, StateName, State) ->
    lists:foreach(fun jb_queue:queue/1, AlbumBrowse#sp_albumbrowse.tracks),
    {next_state, StateName, State};
      
handle_info(_Info, _StateName, State) ->
    lager:warning("<~p> Unhandled message: ~p", [_StateName, _Info]),
    {next_state, _StateName, State}.

terminate(_StateName, _Reason, _State) ->
    ok.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


set_state_stopped(State) ->
    espotify_api:player_unload(),
    State1 = State#state{current_track=undefined},
    jb_web:stream_status(get_status(stopped, State1)),
    {next_state, stopped, State1}.
    
set_state_playing(Track, State) ->
    lager:info("Playing: ~s", [Track#sp_track.name]),
    espotify_api:player_play(true),
    State1 = State#state{current_track=Track,
                         playhead_offset=0,
                         playhead_lastupdate=now_msec()},
    jb_web:stream_status(get_status(playing, State1)),
    {next_state, playing, State1}.



do_try_login(State) ->
    {ok, Username} = application:get_env(jb, spotify_username),
    {ok, Password} = application:get_env(jb, spotify_password),
    ok = espotify_api:start(self(), "/tmp/jb", "/tmp/jb", Username, Password),
    State.


do_handle_next(State) ->
    case jb_queue:pop() of
        {ok, undefined} ->
            %% stop
            set_state_stopped(State);
        {ok, Track} ->
            case espotify_api:player_load(Track#sp_track.link) of
                ok ->
                    {ok, LoadedTrack} = espotify_api:player_current_track(),
                    espotify_api:player_play(true),
                    set_state_playing(LoadedTrack, State);
                loading ->
                    {next_state, loading, State}
            end
    end.


now_msec() ->
    {M,S,Micro} = erlang:now(),
    M*1000000000 + S*1000 + Micro div 1000.

playhead_position(playing, #state{playhead_offset=O, playhead_lastupdate=T}) ->
    O + (now_msec() - T);
playhead_position(paused, #state{playhead_offset=O}) ->
    O;
playhead_position(_, _) ->
    0.
    

get_status(StateName, State) ->
    {StateName,
     playhead_position(StateName, State),
     State#state.current_track}.

