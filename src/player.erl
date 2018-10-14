%%%-------------------------------------------------------------------
%% @author Igor Kopestenski <i.kopest@gmail.com>
%%   [https://github.com/Laymer/GrispLasp/]
%% @doc This is a <em>gen_server</em> template module.
%% @end
%%%-------------------------------------------------------------------

-module(player).

-behaviour(gen_server).

-include("ballgame.hrl").

%% API
-export([start_link/1]).
-export([greet/1]).
-export([greetpid/1]).
-export([shoot/2]).

%% Gen Server Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% Macros
%%====================================================================

% -define(NAME(Number),   list_to_atom(unicode:characters_to_list(["player", "_", integer_to_list(Number)], utf8))).

%%====================================================================
%% Records
%%====================================================================

-record(state, {balls           :: #{atom() => integer()} | #{},
              current           :: integer(),
              others            :: list()}).

%%====================================================================
%% API
%%====================================================================

start_link(Number) ->
    % gen_server:start_link({local, ?GLOBALNAME(Number)}, ?MODULE, [Number], []).
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Number], []).

shoot(Target, Pid) ->
  % gen_server:call(?MODULE, {shoot, Target}).
  gen_server:call(Pid, {shoot, Target}).

greet(Node) ->
  % gen_server:call(?MODULE, {shoot, Target}).
  gen_server:call(?MODULE, {greet, Node}).

greetpid(Node) ->
  % gen_server:call(?MODULE, {shoot, Target}).
  % gen_server:call(?MODULE, {greetpid, Node}).
  partisan_peer_service:forward_message(?MODULE, {greetpid, Node}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init([Number]) ->
    [Others] = [ballgame_util:get(players)] -- [Number],
    % {ok, #state{balls = maps:new(), current = 0}, {continue, {play, Others}}}.
    % {ok,State,{continue,Continue}}.
    {ok, #state{balls = maps:new(), current = 0, others = lists:flatten(Others)}}.

%%--------------------------------------------------------------------

handle_call({shoot, _Target}, _From, _State = #state{balls = B, current = Current, others = Others}) ->
    Manager = ballgame_util:mgr(),
    ok = Manager:forward_message(node(), 1, player_1, {msg, Current}, []),
    % ok = partisan_hyparview_peer_service_manager:forward_message(ballgame@LaymerMac, 1, player, {hello}, []).
    % ok = partisan_hyparview_peer_service_manager:cast_message(N, 1, PidR, {hello}, []).
    % ok = partisan_hyparview_peer_service_manager:send_message(ballgame@LaymerMac, {hello}).
    NewState = #state{balls = B, current = (Current + 1), others = Others},
    {reply, ok, NewState};

%%--------------------------------------------------------------------

handle_call({hello}, From, _State = #state{balls = B, current = Current, others = Others}) ->
    logger:log(notice, "Player ~p said hi ! ~n", [From]),
    % Manager = ballgame_util:mgr(),
    % ok = Manager:forward_message(node(), 1, player_1, {msg, Current}, []),
    NewState = #state{balls = B, current = (Current + 1), others = Others},
    {reply, received_hello, NewState};
% Pid = rpc:call('ballgame@LaymerMac', erlang, list_to_pid, ["<0.365.0>"]).
%%--------------------------------------------------------------------

handle_call({greet, Node}, _From, _State = #state{balls = B, current = Current, others = Others}) ->
    logger:log(notice, "Saying hello to Player ~p ! ~n", [Node]),
    % Manager = ballgame_util:mgr(),
    partisan_peer_service:forward_message(Node,player,{hello}),
    % partisan_peer_service:cast_message(Node,player,{hello}).
    % partisan_peer_service:forward_message(ballgame@LaymerMac,player,{hello}).
    % partisan_peer_service:cast_message(ballgame@LaymerMac,player,{hello}).
    % ok = Manager:forward_message(Node, 1, player_1, {msg, Current}, []),
    NewState = #state{balls = B, current = (Current + 1), others = Others},
    {reply, said_hello, NewState};

%%--------------------------------------------------------------------

handle_call({greetpid, Node}, _From, _State = #state{balls = B, current = Current, others = Others}) ->
    logger:log(notice, "Saying hello to Player ~p ! ~n", [Node]),
    Manager = ballgame_util:mgr(),
    Pid = rpc:call(Node, erlang, whereis, [player]),
    % partisan_peer_service:forward_message(Node,player,{hello}),
    % partisan_peer_service:cast_message(Node,player,{hello}).
    % partisan_peer_service:forward_message(ballgame@LaymerMac,player,{hello}).
    % partisan_peer_service:cast_message(ballgame@LaymerMac,player,{hello}).
    ok = Manager:forward_message(Node, 1, Pid, {hello}, []),
    NewState = #state{balls = B, current = (Current + 1), others = Others},
    {reply, said_hello, NewState};

%%--------------------------------------------------------------------

handle_call(Request, From, State) ->
    logger:log(notice, "Received unhandled ~p request from ~p ! ~n", [Request,From]),
    {reply, ignored, State}.

%%--------------------------------------------------------------------

handle_cast({hello}, State) ->
    logger:log(notice, "Player casted hi ! ~n"),
    {noreply, State};
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(Info, State) ->
    logger:log(notice, "Received INFO : ~p ! ~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

%% This function is called by a gen_server process
%% whenever a previous callback returns {continue, Continue}.
%% handle_continue/2 is invoked immediately after the previous callback,
%% which makes it useful for performing work after initialization
%% or for splitting the work in a callback in multiple steps,
%% updating the process state along the way.

%%--------------------------------------------------------------------

handle_continue({continue, _Continue}, State) ->
    % {noreply, State};
    {noreply, State}.
% handle_continue({continue, {play, Others}}, State) ->
% handle_continue({continue, Continue}, State) ->
  % {noreply,NewState} | {noreply,NewState,Timeout}
  % | {noreply,NewState,hibernate}
  % | {noreply,NewState,{continue,Continue}}
  % | {stop,Reason,NewState}

    % case Continue of
    %   {play, Target} ->
    %     logger:log(info, "Player ~p will shoot in 3 seconds~n", [node()]),
    %
    %     ?PAUSE3,
    %     shoot(Target);
    %   _ ->
    % {noreply, State}.
    % end.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
