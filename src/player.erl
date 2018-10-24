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
-export([start_link/3]).
-export([stress/1]).
-export([shoot/1]).
-export([play/1]).
-export([set_ops_count/1]).

%% Gen Server Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         terminate/2,
         code_change/3]).

%% Test Func
% -export([init_test/0]).
-compile(export_all).
%%====================================================================
%% Macros
%%====================================================================

-define(SERVICE,   partisan_peer_service).
-define(HYPAR,      partisan_hyparview_peer_service_manager).

% -define(ballgame_util:get(stress_delay),      ballgame_util:get(stress_ballgame_util:get(stress_delay))).
% -define(ballgame_util:get(operations_count),      ballgame_util:get(operations_count)).
% -define(PEER,      partisan_hyparview_peer_service_manager).
% -define(NAME(Number),   list_to_atom(unicode:characters_to_list(["player", "_", integer_to_list(Number)], utf8))).

%%====================================================================
%% Records
%%====================================================================


-record(state, {channel :: pos_integer(),
                ball :: first(),
                name :: atom(),
                set :: lasp_id(),
                ops :: pos_integer(),
                stress_delay :: pos_integer(),
                packet :: bitstring(),
                packet_size :: pos_integer(),
                remote :: atom()}).

-type lasp_id()       :: { bitstring(), atom() }.
-type stress_state() :: #state{}.
-type first()        :: false|true.

%%====================================================================
%% API
%%====================================================================

-spec start_link({awset|stress_test, first()|map()}) -> {ok, pid()}.
start_link({stress_test, First}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {stress_test, First}, []);
% -spec start_link({awset, map()}) -> {ok, pid()}.
start_link({awset, PacketConfig}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {awset, PacketConfig}, []).

-spec start_link({stress_test, first()}, atom(), pos_integer()) -> {ok, pid()}.
start_link({stress_test, First}, Name, Channel) ->
    gen_server:start_link({local, Name}, ?MODULE, {stress_test, First, Name, Channel}, []).


shoot(Target) ->
    gen_server:call(?MODULE, {shoot, Target}).

play(Target) ->
    gen_server:call(?MODULE, {play, Target}).

stress(Remote) ->
    gen_server:cast(?MODULE, {stress, Remote}).

awset() ->
    gen_server:call(?MODULE, awset).

heavy_awset() ->
    gen_server:call(?MODULE, heavy_awset).

get_awset() ->
    gen_server:call(?MODULE, get_awset).

set_ops_count(Ops) ->
    gen_server:call(?MODULE, {set_ops_count, Ops}).
    % gen_server:call(whereis(player1), awset).
    % gen_server:cast(whereis(player1), {stress, ballgame@Laymer540}).
%%====================================================================
%% Gen Server Callbacks
%%====================================================================

% -spec init({stress_test, first()}) -> {'ok', stress_state()}.
init({awset, PacketConfig}) when is_map(PacketConfig) ->
    % logger:log(notice, "Name = ~p  ! ~n", [Name]),
    {ok, #state{ops = maps:get(operations_count, PacketConfig)
        , channel = maps:get(channel, PacketConfig)
        , name = ?MODULE
        , ball = false
        , remote = node()
        , stress_delay = maps:get(stress_delay, PacketConfig)
        , packet_size = maps:get(packet_size, PacketConfig)}};
init({stress_test, First}) ->
    {ok, #state{ops = ballgame_util:get(operations_count), channel = 1, ball = First, remote = node()}};
init({stress_test, First, Name}) ->
    % logger:log(notice, "Name = ~p  ! ~n", [Name]),
    {ok, #state{ops = ballgame_util:get(operations_count), channel = 1, ball = First, remote = node()}}.
%%--------------------------------------------------------------------

handle_call(awset, _From, State) ->
    logger:log(notice, "AWSET request ! ~n"),
    Id = ballgame_util:declare_awset(set),
    erlang:send_after(?ONE,self(),<<"add">>),
    {reply, ok, State#state{set = Id}};
%%--------------------------------------------------------------------

handle_call({set_ops_count, Ops}, _From, State) ->
    logger:log(notice, "RESET OPS ! ~n"),
    {reply, ok, State#state{ops = Ops}};

%%--------------------------------------------------------------------

handle_call(heavy_awset, _From, State) ->
    logger:log(notice, "AWSET WITH PACKET request ! ~n"),
    Id = ballgame_util:declare_awset(set),
    Packet = ballgame_util:get_packet(State#state.packet_size),
    logger:log(notice, "PACKET =  ~p Size ~p bits ~n",[Packet, bit_size(Packet)]),
    erlang:send_after(?ONE,self(),<<"add">>),
    {reply, ok, State#state{set = Id, packet = Packet}};

%%--------------------------------------------------------------------

handle_call(get_awset, _From, State) ->
    logger:log(notice, "GET AWSET request ! ~n"),
    % Id = ballgame_util:declare_awset(set),
    % erlang:send_after(?ONE,self(),<<"add">>),
    Name = ballgame_util:get(awset),
    {ok, Set} = lasp:query(Name),
    {reply, sets:to_list(Set), State};

%%--------------------------------------------------------------------

handle_call(Request, From, State) ->
    logger:log(notice, "Received unhandled ~p request from ~p ! ~n", [Request,From]),
    {reply, ignored, State}.

%%--------------------------------------------------------------------

-spec handle_cast({stress,atom()}|{hello}, stress_state()) -> {'noreply', stress_state()}.
handle_cast({hello}, State) ->
    logger:log(notice, "Player casted hi ! ~n"),
    {noreply, State};

%%--------------------------------------------------------------------

% handle_cast({ball, Number, Player}, State = #state{received = Rcv}) ->
%     logger:log(notice, "Received a ball with number ~p from player ~p ! ~n",[Number,Player]),
%     % NewState = State#state{received = (Rcv + 1)},
%     NewState = State#state{received = (State#state.received+1)},
%     {noreply, NewState};
% handle_cast({stress, Remote}, State) ->
%     logger:log(notice, "Stressing ~p ! ~n", [Remote]),
%     ok = ?HYPAR:forward_message(Remote, 1, player, <<1:1>>, []),
%     {reply, ok, }.
handle_cast({stress,Remote}, State) ->
    % ?HYPAR:forward_message(Remote, 1, player, {<<1:1>>,node()}, []),
    ?HYPAR:forward_message(Remote, State#state.channel, State#state.name, {<<1:1>>,node()}, []),
    {noreply, State#state{ball = not State#state.ball, remote = Remote}};
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------


-spec handle_info(any(), stress_state()) -> {'noreply', stress_state()}.
handle_info({<<1:1>>,Remote}, State) ->
    % ?HYPAR:forward_message(Remote, 1, player, <<1:1>>, []),
    ?HYPAR:forward_message(Remote, State#state.channel, State#state.name, {<<1:1>>,node()}, []),
    {noreply, State#state{remote = Remote}};

handle_info(<<1:1>>, State) ->
    % logger:log(notice, "BINARY BALL EXCHANGE ! ~n"),
    % ?PAUSE1,
    timer:sleep(ballgame_util:get(stress_delay)),
    % ?HYPAR:forward_message(State#state.remote, 1, player, <<1:1>>, []),
    ?HYPAR:forward_message(State#state.remote, State#state.channel, State#state.name, <<1:1>>, []),
    {noreply, State};

handle_info(<<"add">>, State) when State#state.ops > 0 ->
    logger:log(info, "ADD ! ~n"),
    logger:log(info, "PACKET =  ~p Size ~p bits ~n",[State#state.packet, bit_size(State#state.packet)]),
    % ?PAUSE1,
    % timer:sleep(ballgame_util:get(stress_delay)),
    lasp:update(State#state.set, {add, State#state.packet}, self()),
    erlang:send_after(State#state.stress_delay,self(),<<"rmv">>),
    % erlang:send_after(10,self(),<<"rmv">>),
    % ?HYPAR:forward_message(State#state.remote, 1, player, <<1:1>>, []),
    % ?HYPAR:forward_message(State#state.remote, State#state.channel, State#state.name, <<1:1>>, []),
    % {noreply, State#state{set = NewSet}};
    {noreply, State};

handle_info(<<"add">>, State) ->
    logger:log(notice, "FINISHED ! ~n"),
    % ?PAUSE1,
    % timer:sleep(ballgame_util:get(stress_delay)),
    % lasp:update(State#state.set, {add, <<1:1>>}, self()),
    % erlang:send_after(ballgame_util:get(stress_delay),self(),<<"rmv">>),
    % erlang:send_after(10,self(),<<"rmv">>),
    % ?HYPAR:forward_message(State#state.remote, 1, player, <<1:1>>, []),
    % ?HYPAR:forward_message(State#state.remote, State#state.channel, State#state.name, <<1:1>>, []),
    % {noreply, State#state{set = NewSet}};
    {noreply, State};

handle_info(<<"rmv">>, State) ->
    % logger:log(notice, "RMV ! ~n"),
    % ?PAUSE1,
    % timer:sleep(ballgame_util:get(stress_delay)),
    % {ok, {NewSet, _, _, _}} = lasp:update(State#state.set, {rmv, <<1:1>>}, self()),
    % lasp:update(State#state.set, {rmv, <<1:1>>}, self()),
    lasp:update(State#state.set, {rmv, State#state.packet}, self()),
    erlang:send_after(State#state.stress_delay,self(),<<"add">>),
    % erlang:send_after(10,self(),<<"add">>),
    % ?HYPAR:forward_message(State#state.remote, 1, player, <<1:1>>, []),
    % ?HYPAR:forward_message(State#state.remote, State#state.channel, State#state.name, <<1:1>>, []),
    % {noreply, State#state{set = NewSet}};
    {noreply, State#state{ops = State#state.ops - 1}};

handle_info({rc}, State) ->
    logger:log(notice, "Received INFO : RC ! ~n"),
    ballgame_util:seek_neighbors(),
    {noreply, State};
%
% handle_info(timeout, State) ->
%     logger:log(notice, "Received INFO : <<packet>> ! ~n"),
%     logger:log(notice, "Initializing Packet ! ~n"),
%     Stress_delay = ballgame_util:get(stress_delay),
%     Packet_size = ballgame_util:get(packet_size),
%     Packet = ballgame_util:get_packet(Packet_size),
%     {noreply, State#state{packet = Packet
%     , stress_delay = Stress_delay
%     , packet_size = Packet_size}};
    % {noreply, State};


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


    % erlang:send_after(500,?MODULE,{rc}),
    % case Args of
    %   {stress_test, First} ->
    %     {ok, #state{channel = 0, ball = First}};
    %   _ ->
    % end.

%%--------------------------------------------------------------------


%%--------------------------------------------------------------------

% handle_info({ball,Player}, State = #state_ball{has_ball = Catched}) ->
%     NewState = State#state_ball{has_ball = not Catched},
%     logger:log(notice, "Received ball from : ~p ! ~n", [Player]),
%     ?PAUSE1,
%     (ballgame_util:mgr()):forward_message(Player, 1, player, {ball, node()}, []),
%     {noreply, NewState};
%%--------------------------------------------------------------------

% handle_info({ball,Player}, State = #state_ball{has_ball = Catched}) ->
%     NewState = State#state_ball{has_ball = not Catched},
%     logger:log(notice, "Received ball from : ~p ! ~n", [Player]),
%     ?PAUSE1,
%     (ballgame_util:mgr()):forward_message(Player, 1, player, {ball, node()}, []),
%     {noreply, NewState};

%%--------------------------------------------------------------------

%%--------------------------------------------------------------------

% handle_call({shoot, Target}, _From, State = #state{current = Current, others = _Others}) ->
%     Manager = ballgame_util:mgr(),
%     ok = Manager:cast_message(Target, 1, player, {ball, Current, node()}, []),
%     NewState = State#state{current = (Current + 1)},
%     {reply, ok, NewState};

%%--------------------------------------------------------------------

% handle_call({play, Target}, _From, State = #state_ball{has_ball = FirstHand}) ->
%     NewState = State#state_ball{has_ball = not FirstHand},
%     (ballgame_util:mgr()):forward_message(Target, 1, player, {ball, node()}, []),
%     {noreply, NewState};

%%--------------------------------------------------------------------

% handle_call({hello}, From, State = #state{current = Current, others = _Others}) ->
%     logger:log(notice, "Player ~p said hi ! ~n", [From]),
%     NewState = State#state{current = (Current + 1)},
%     {reply, received_hello, NewState};

%%--------------------------------------------------------------------

% handle_call({greet, Node}, _From, State = #state{current = Current, others = _Others}) ->
%     logger:log(notice, "Saying hello to Player ~p ! ~n", [Node]),
%     NewState = State#state{current = (Current + 1)},
%     ?P:forward_message(Node,player,{hello}),
%     {reply, said_hello, NewState};

%%--------------------------------------------------------------------

% handle_call({greetpid, Node}, _From, State = #state{current = Current, others = _Others}) ->
%     logger:log(notice, "Saying hello to Player ~p ! ~n", [Node]),
%     Manager = ballgame_util:mgr(),
%     % Pid = rpc:call('ballgame@LaymerMac', erlang, list_to_pid, ["<0.365.0>"]).
%     Pid = rpc:call(Node, erlang, whereis, [player]),
%     ok = Manager:forward_message(Node, 1, Pid, {hello}, []),
%     NewState = State#state{current = (Current + 1)},
%     {reply, said_hello, NewState};



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
