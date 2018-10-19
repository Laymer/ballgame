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
-export([shoot/1]).
-export([play/1]).

%% Gen Server Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         terminate/2,
         code_change/3]).

%% Test Func
-export([init_test/0]).

%%====================================================================
%% Macros
%%====================================================================

-define(P,  partisan_peer_service).
% -define(NAME(Number),   list_to_atom(unicode:characters_to_list(["player", "_", integer_to_list(Number)], utf8))).

%%====================================================================
%% Records
%%====================================================================

-record(state,  {current           :: integer(),
                received           :: integer(),
                others             :: list()}).

-record(state_ball,  {has_ball     :: boolean()}).

%%====================================================================
%% API
%%====================================================================

init_test() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [{stress_test,true}], []).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

shoot(Target) ->
    gen_server:call(?MODULE, {shoot, Target}).

play(Target) ->
    gen_server:call(?MODULE, {play, Target}).

greet(Node) ->
    gen_server:call(?MODULE, {greet, Node}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init([Args]) ->
    case Args of
      {stress_test, First} ->
        {ok, #state_ball{has_ball = First}};
      _ ->
        {ok, #state{current = 0, received = 0, others = []}}
    end.

%%--------------------------------------------------------------------

handle_call({shoot, Target}, _From, State = #state{current = Current, others = _Others}) ->
    Manager = ballgame_util:mgr(),
    % ok = Manager:cast_message(Target, 1, player, {ball, Current, node()}, []),
    ok = Manager:cast_message(Target, 1, player, {ball}, []),
    NewState = State#state{current = (Current + 1)},
    {reply, ok, NewState};

%%--------------------------------------------------------------------

handle_call({play, Target}, _From, State = #state_ball{has_ball = FirstHand}) ->
    NewState = State#state_ball{has_ball = not FirstHand},
    (ballgame_util:mgr()):forward_message(Target, 1, player, {ball, node()}, []),
    {noreply, NewState};

%%--------------------------------------------------------------------

handle_call({hello}, From, State = #state{current = Current, others = _Others}) ->
    logger:log(notice, "Player ~p said hi ! ~n", [From]),
    NewState = State#state{current = (Current + 1)},
    {reply, received_hello, NewState};

%%--------------------------------------------------------------------

handle_call({greet, Node}, _From, State = #state{current = Current, others = _Others}) ->
    logger:log(notice, "Saying hello to Player ~p ! ~n", [Node]),
    NewState = State#state{current = (Current + 1)},
    ?P:forward_message(Node,player,{hello}),
    {reply, said_hello, NewState};

%%--------------------------------------------------------------------

handle_call({greetpid, Node}, _From, State = #state{current = Current, others = _Others}) ->
    logger:log(notice, "Saying hello to Player ~p ! ~n", [Node]),
    Manager = ballgame_util:mgr(),
    % Pid = rpc:call('ballgame@LaymerMac', erlang, list_to_pid, ["<0.365.0>"]).
    Pid = rpc:call(Node, erlang, whereis, [player]),
    ok = Manager:forward_message(Node, 1, Pid, {hello}, []),
    NewState = State#state{current = (Current + 1)},
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

handle_cast({ball, Number, Player}, State = #state{received = Rcv}) ->
    logger:log(notice, "Received a ball with number ~p from player ~p ! ~n",[Number,Player]),
    % NewState = State#state{received = (Rcv + 1)},
    NewState = State#state{received = (State#state.received+1)},
    {noreply, NewState};

%%--------------------------------------------------------------------

handle_cast({ball}, State = #state{received = Rcv}) ->
    logger:log(notice, "Received a ball ! ~n"),
    erlang:send_after(?ONE,self(),{shoot, hd(nodes())}),
    % NewState = State#state{received = (Rcv + 1)},
    NewState = State#state{received = (State#state.received+1)},
    {noreply, NewState};

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info({ball,Player}, State = #state_ball{has_ball = Catched}) ->
    NewState = State#state_ball{has_ball = not Catched},
    logger:log(notice, "Received ball from : ~p ! ~n", [Player]),
    ?PAUSE1,
    (ballgame_util:mgr()):forward_message(Player, 1, player, {ball, node()}, []),
    {noreply, NewState};

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




%%====================================================================
%% Snippets
%%====================================================================

% ok = Manager:forward_message(node(), 1, player_1, {msg, Current}, []),
% ok = partisan_hyparview_peer_service_manager:forward_message(ballgame@LaymerMac, 1, player, {hello}, []).
% ok = partisan_hyparview_peer_service_manager:cast_message(N, 1, PidR, {hello}, []).
% ok = partisan_hyparview_peer_service_manager:send_message(ballgame@LaymerMac, {hello}).

% ok = Manager:forward_message(node(), 1, player_1, {msg, Current}, []),
% ok = partisan_hyparview_peer_service_manager:forward_message(ballgame@LaymerMac, 1, player, {hello}, []).
% ok = partisan_hyparview_peer_service_manager:cast_message(N, 1, PidR, {hello}, []).
% ok = partisan_hyparview_peer_service_manager:send_message(ballgame@LaymerMac, {hello}).

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

% partisan_peer_service:cast_message(Node,player,{hello}).
% partisan_peer_service:forward_message(ballgame@LaymerMac,player,{hello}).
% partisan_peer_service:cast_message(ballgame@LaymerMac,player,{hello}).
% ok = Manager:forward_message(Node, 1, player_1, {msg, Current}, []),
% partisan_peer_service:forward_message(Node,player,{hello}),
% partisan_peer_service:cast_message(Node,player,{hello}).
% partisan_peer_service:forward_message(ballgame@LaymerMac,player,{hello}).
% partisan_peer_service:cast_message(ballgame@LaymerMac,player,{hello}).
