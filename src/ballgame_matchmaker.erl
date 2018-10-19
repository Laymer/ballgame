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
-export([run/0]).
-export([check/0]).


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

-spec run() -> ok.
run() ->
    gen_server:cast(?MODULE, <<"run">>).

-spec check() -> ok.
check() ->
    gen_server:call(?MODULE, <<"check">>).


%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init([]) ->
  erlang:send_after(?THREE, ?MODULE, <<"check">>),
  {ok, []}.

%%--------------------------------------------------------------------

handle_call(<<"check">>, _From, []) ->
    % N = ballgame_util:maybe_get_first(State),

    Members = ballgame_util:clusterize(),
    % case length(Members) > 0 of
    %     true ->
    %         Members;
    %     false ->
    %         logger:log(notice, "No remote ! ~n"),
    %         logger:log(notice, "Retrying .... ~n"),
    %         []
    % end,
    erlang:send_after(?THREE, ?MODULE, <<"check">>),
    {reply, checked, Members}.

%%--------------------------------------------------------------------

handle_cast(<<"run">>, State) ->
    Alone = ballgame_util:alone(),
    case Alone of
        false ->
            {stress,player:stress(hd(nodes()))};
        _ ->
            erlang:send_after(?THREE, ?MODULE, <<"check">>),
            {stress,erralone}
    end,
    % M = ballgame_util:members(),
    % logger:log(notice, "Available remotes for stress test : ~n"),
    % logger:log(notice, "Nodes ~p : ~n",[M]),
    %
   {noreply, State}.

%%--------------------------------------------------------------------

handle_info({refresh}, _State) ->
  grisp_led:pattern(1, [{100, rand_pattern()}]),
  NewMembers = ballgame_util:clusterize(),
  erlang:send_after(?MIN, self(), {refresh}),
  % grisp_led:off(1),
  % grisp_led:off(2),
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
