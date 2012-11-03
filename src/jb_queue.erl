-module(jb_queue).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("espotify/include/espotify.hrl").

-record(state, {queue=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         pop/0,
         queue/1,
         track_loaded/1,
         get_queue/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

pop() ->
    gen_server:call(?SERVER, pop).

queue("spotify:track:" ++ _ = Link) ->
    queue(#sp_track{link=Link});
queue(Track=#sp_track{}) ->
    gen_server:call(?SERVER, {queue, Track}).

track_loaded(Track=#sp_track{}) ->
    gen_server:cast(?SERVER, {track_loaded, Track}).

get_queue() ->
    gen_server:call(?SERVER, get_queue).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(pop, _From, State) ->
    {Reply, Queue1} = do_pop(State#state.queue),
    {reply, Reply, State#state{queue=Queue1}};

handle_call({queue, Track}, _From, State=#state{queue=Q}) ->
    Queue1 = do_queue(Track, Q),
    {reply, ok, State#state{queue=Queue1}};
    
handle_call(get_queue, _From, State=#state{queue=Q}) ->
    {reply, {ok, Q}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({track_loaded, Track}, State) ->
    Queue1 = replace_loaded_track(Track, State#state.queue),
    {noreply, State#state{queue=Queue1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    lager:warning("Unhandled info: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_pop([]) ->
    %% default song
    {{ok, #sp_track{link="spotify:track:6JEK0CvvjDjjMUBFoXShNZ"}}, []};

%% only play songs that have been loaded.
do_pop([#sp_track{is_loaded = false}|Rest]) ->
    do_pop(Rest);

do_pop([Track|Rest]) ->
    {{ok, Track}, Rest}.


do_queue(Track, Queue) ->
    case Track#sp_track.is_loaded of
        false ->
            espotify_api:track_info(Track#sp_track.link);
        true ->
            nop
    end,
    Queue++[Track].

%% A track has loaded; replace it in the queue
replace_loaded_track(LoadedTrack, Queue) ->
    Link = LoadedTrack#sp_track.link,
    lists:reverse(
      lists:foldl(
        fun(T, Acc) ->
                [case T#sp_track.link of
                     Link ->
                         LoadedTrack;
                     _ ->
                         T
                 end | Acc]
        end,
        [],
        Queue)
     ).
