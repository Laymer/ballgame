%%%-------------------------------------------------------------------
%% @doc ballgame public API
%% @end
%%%-------------------------------------------------------------------

-module(ballgame_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Macros
%%====================================================================



%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    % prepare_game(),
    % {ok, Supervisor} = ballgame_sup:start_link(awset),
    {ok, Supervisor} = ballgame_sup:start_link(nolasp),
    % {ok, Supervisor} = ballgame_sup:start_link(),
    logger:log(info, "The game is about to start"),
    LEDs = [1, 2],
    [grisp_led:flash(L, red, 500) || L <- LEDs],
    timer:sleep(5000),
    % grisp_led:off(2),
    % Random = fun() ->
    %     {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
    % end,
    % grisp_led:pattern(1, [{100, Random}]),
    % inetconf(),
    {ok, Supervisor}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

prepare_game() ->
  logger:log(info, "Preparing cluster"),

  {ok, _} = application:ensure_all_started(partisan).

inetconf() ->
  logger:log(info, "Preparing DB"),
  whereis(inet_db).
  % erlang:send_after(200,self(),{rc}).
  % {ok, L} = inet:getif(),
  % {{_,_,_,Num},_,_} = hd(L),
  % application:set_env(ballgame,number,Num).
  % Maker = ballgame_util:get(matchmaker),
  % Maker:start().
