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
-export([start/0]).

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

start() ->
   gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init([]) ->
  clusterize(),
  {ok, #{}}.

%%--------------------------------------------------------------------

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

%%====================================================================
%% Clustering functions
%%====================================================================

join(Host) ->
  Manager = rpc:call(Host, partisan_peer_service, manager, []),
  case Manager of
    partisan_hyparview_peer_service_manager ->
      Node = rpc:call(Host, Manager, myself, []),
      ok = partisan_peer_service:join(Node),
      logger:log(info, "Joined ~p~n", [Host]),
      Node;
    {error, Reason} ->
      logger:log(error, "Unable to retrieve remote : ~p~n", [Manager]),
      {error, Reason}
  end.

clusterize() ->
  logger:log(info, "Joining reachable nodes ~n"),
  [Numbers] = ballgame_util:get(players),
  Team = ?TEAM(Numbers),
  _L = [ join(X) ||
      X <- Team,
      X =/= node(),
      net_adm:ping(X) =:= pong ],
  ballgame_util:members().
