-module(jb_player).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

-include_lib("deps/espotify/include/espotify.hrl").

-record(state,
        {current_track = undefined :: undefined | #sp_track{},
         spotify_user = undefined :: undefined | #sp_user{},
         playlist_container = undefined  :: undefined | #sp_playlistcontainer{}
        }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/0,
         next/0,
         stop/0,
         toggle_pause/0
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

toggle_pause() ->
    gen_fsm:send_all_state_event(?SERVER, toggle_pause).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, unauthorized, #state{}, 0}.

%% @doc Initial login
unauthorized(timeout, State) ->
    {next_state, unauthorized, do_try_login(State)}.

handle_event(next, _StateName, State) ->
    {ok, Track} = jb_queue:pop(),
    handle_load_and_play(Track, State);

handle_event(toggle_pause, playing, State) ->
    espotify_api:player_play(false),
    {next_state, paused, State};

handle_event(toggle_pause, paused, State) ->
    espotify_api:player_play(true),
    {next_state, playing, State};

handle_event(stop, S, State) when S =:= playing; S =:= paused->
    espotify_api:player_unload(),
    {next_state, stopped, State#state{current_track=undefined}};

handle_event(_Event, _StateName, State) ->
    lager:warning("<~p> Unhandled event: ~p", [_StateName, _Event]),
    {next_state, _StateName, State}.

handle_sync_event(_Event, _From, _StateName, State) ->
    lager:warning("Unhandled sync event: ~p", [_Event]),
    {next_state, _StateName, State}.

handle_info({'$spotify_callback', logged_in, R}, _StateName, State) ->
    case R of
        {ok, User} ->
            lager:warning("User: ~p", [User]),
            {next_state, stopped, State#state{spotify_user=User}};
        {error, E} ->
            lager:error("Error: ~p", [E]),
            espotify_api:stop(),
            {next_state, unauthorized, State, 60000}
    end;
             
handle_info({'$spotify_callback', load_playlistcontainer, {ok, {undefined, PC}}}, StateName, State) ->
    lager:warning("recv playlist"),
    {next_state, StateName, State#state{playlist_container=PC}};

handle_info({'$spotify_callback', player_load, {ok, Track}}, loading, State) ->
    espotify_api:player_play(true),
    {next_state, playing, State#state{current_track=Track}};

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

do_try_login(State) ->
    {ok, Username} = application:get_env(jb, spotify_username),
    {ok, Password} = application:get_env(jb, spotify_password),
    ok = espotify_api:start(self(), "/tmp/jb", "/tmp/jb", Username, Password),
    State.


handle_load_and_play(Track, State) ->
    State1 = State#state{current_track=Track},
    case espotify_api:player_load(Track#sp_track.link) of
        ok ->
            espotify_api:player_play(true),
            {next_state, playing, State1};
        loading ->
            {next_state, loading, State1}
    end.

