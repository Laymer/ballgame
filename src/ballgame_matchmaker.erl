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
%% Gen Server Callbacks
%%====================================================================

init([]) ->
  % Members = ballgame_util:clusterize(),
  erlang:send_after(?TEN, self(), {refresh}),
  {ok, #{members => []}}.

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
   {reply, ignored, State}.

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------

handle_info({refresh}, _State) ->
  grisp_led:pattern(1, [{100, rand_pattern()}]),
  NewMembers = ballgame_util:clusterize(),
  erlang:send_after(?MIN, self(), {refresh}),
  grisp_led:off(1),
  grisp_led:off(2),
  {noreply, #{members => NewMembers}};

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
   {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
   ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

 rand_pattern() ->
     {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}.
