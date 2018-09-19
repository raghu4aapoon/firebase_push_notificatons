%%%-------------------------------------------------------------------
%% @doc fcm public API
%% @end
%%%-------------------------------------------------------------------

-module(fcm_app).

-behaviour(application).

%% API
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(pegb_wallet_core),
  ok.

-spec stop() -> ok.
stop() ->
  application:stop(pegb_wallet_core).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @hidden
start(_StartType, _StartArgs) ->
    fcm_sup:start_link().

stop(_State) ->
    ok.
