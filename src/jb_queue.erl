-module(jb_queue).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("deps/espotify/include/espotify.hrl").

-record(state, {queue=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, pop/0, queue/1]).

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
    
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, #state{}}.

handle_call(pop, _From, State=#state{queue=[Track|Rest]}) ->
    {reply, {ok, Track}, State#state{queue=Rest}};

handle_call(pop, _From, State=#state{queue=[]}) ->
    Track = #sp_track{link="spotify:track:6JEK0CvvjDjjMUBFoXShNZ"},
    {reply, {ok, Track}, State};

handle_call({queue, Track}, _From, State=#state{queue=Q}) ->
    {reply, ok, State#state{queue=Q++[Track]}};
    
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

