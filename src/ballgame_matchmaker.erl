%%%-------------------------------------------------------------------
%% @author Igor Kopestenski <i.kopest@gmail.com>
%%   [https://github.com/Laymer/GrispLasp/]
%% @doc This module contains utilities for Partisan clustering.
%% @end
%%%-------------------------------------------------------------------

-module(ballgame_matchmaker).

-behaviour(gen_server).

-include("ballgame.hrl").

%% API
-export([start_link/0]).
-export([clusterize/0]).

%% Gen Server Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
   gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Clustering functions
%%====================================================================


clusterize() ->
  % gen_server:call(?MODULE, {shoot, Target}).
  gen_server:call(?MODULE, {clusterize}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init([]) ->
  % clusterize(),
  {ok, #{}}.

%%--------------------------------------------------------------------

handle_call({clusterize}, _From, State) ->
    logger:log(info, "Joining reachable nodes ~n"),
    % [Numbers] = ballgame_util:get(players),
    [Numbers] = ballgame_util:get(fakeplayers),
    % Team = ?TEAM(Numbers),
    Team = ?FAKETEAM(Numbers),
    % _L = [ ballgame_util:join(X) ||
    L = [ ballgame_util:join(X) ||
        X <- Team,
        X =/= node(),
        net_adm:ping(X) =:= pong ],
    % M = ballgame_util:members(),
    % logger:log(info, "Joined = ~p ~n", [M]),
    logger:log(info, "Joined = ~p ~n", [L]),
   {reply, L, State};

handle_call(_Request, _From, State) ->
   {reply, ignored, State}.

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
   ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
