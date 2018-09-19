%%%-------------------------------------------------------------------
%% @doc fcm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(fcm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @hidden
init([]) ->
  Child = [fcm:child_spec()],
  {ok, { {one_for_all, 0, 1}, Child} }.
