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
-export([spawn_players/0]).
-export([check/0]).

-define(SERVER, ?MODULE).

%% Gen Server Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% Records
%%====================================================================


-record(state, {players :: list(),
                channels :: list()}).

-type stress_state() :: #state{}.
%%====================================================================
%% API
%%====================================================================

start_link() ->
   gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec run() -> ok.
run() ->
    gen_server:cast(?MODULE, <<"run_awset">>).

-spec spawn_players() -> {ok, list()}|{error, atom()}.
spawn_players() ->
    % Num = ballgame_util:get(players),
    % Players = ?PLAYER_TEAM(Num),
    % gen_server:call(?SERVER, {spawn,Players}).
    gen_server:call(?SERVER, spawn).

-spec check() -> ok.
check() ->
    gen_server:call(?MODULE, <<"check">>).


%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init([]) ->
  % erlang:send_after(?THREE, ?MODULE, <<"check">>),
  Channels = partisan_config:get(channels),
  Num = ballgame_util:get(players),
  Players = case Num of
    1 ->
      [player];
    _ when Num > 1 ->
      ?PLAYER_TEAM(Num);
    _ ->
      []
    end,
  % Zip = lists:zip(Players, Channels),
  % State = #state{players = Players, channels = Channels},
  State = #state{players = Players, channels = Channels},
  {ok, State}.

%%--------------------------------------------------------------------

handle_call(spawn, _From, State) ->
    % L = [ {player:start_link({stress_test, true}, X),X,1}
    %     || X <- Players
    %     , C <- State#state.channels ],
    L = [ player:start_link({stress_test, true}, P, C)
        || {P,C} <- State#state.channels ],

    % [{{ok, Pid}, Name},{{ok, Pid}, Name2}, ...]
    % {ok,[{{ok,<0.587.0>},player1,1},{{ok,<0.588.0>},player2,2}]}.

    {reply, {ok, L}, State};
%%--------------------------------------------------------------------

handle_call(<<"check">>, _From, []) ->
    % N = ballgame_util:maybe_get_first(State),

    % Members = ballgame_util:clusterize(),
    % case length(Members) > 0 of
    %     true ->
    %         Members;
    %     false ->
    %         logger:log(notice, "No remote ! ~n"),
    %         logger:log(notice, "Retrying .... ~n"),
    %         []
    % end,
    erlang:send_after(?THREE, ?MODULE, <<"check">>),
    {noreply, []}.

%%--------------------------------------------------------------------

handle_cast(<<"run_awset">>, State) ->
    [ gen_server:call(whereis(X),awset) || X <- State#state.players ],
    % Alone = ballgame_util:alone(),
    % case Alone of
    %     false ->
    %         {stress,player:stress(hd(nodes()))};
    %     _ ->
    %         erlang:send_after(?THREE, ?MODULE, <<"check">>),
    %         {stress,erralone}
    % end,
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



%%====================================================================
%% Internal functions
%%====================================================================



 rand_pattern() ->
     {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}.
