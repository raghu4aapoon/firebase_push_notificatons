-module(fcm).

-behaviour(gen_server).

%% API
-export([
  child_spec/0,
  start_link/0,
  push/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2
]).

-define(TAG, fcm_notifications).

-type fcm_payload() :: map().

%%%===================================================================
%%% API
%%%===================================================================

%% @hidden
child_spec() ->
  #{
    id      => ?MODULE,
    start   => {?MODULE, start_link, []},
    restart => permanent
  }.

-spec push(Payload) -> Res when
  Payload :: fcm_payload(),
  Res     :: ok.
push(Payload) ->
  gen_server:cast(?MODULE, {push, Payload}).

-spec start_link() -> Res when
  Res :: {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([]) ->
  {ok, FireBaseConfig} = application:get_env(fcm, firebase),
  #{url := Url, api_key := APIKey} = FireBaseConfig,
  {ok, #{url => Url, auth_key => <<"key=", APIKey/binary>>}}.

%% @hidden
handle_cast({push, Payload}, State) ->
  ok = send(Payload, State),
  {noreply, State}.

%% @hidden
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send(Payload, #{url := Url , auth_key := Key}) ->
  case hackney:post(Url, headers(Key), fcm_utils:encode(Payload), options()) of
    {ok, 200, _RespHeaders, RespBody} ->
      LogMsg = " ~p: successful request sent  RespBody ~p",
      lager:info(LogMsg, [?TAG, fcm_utils:decode(RespBody)]);
    {ok, StatusCode, RespHeaders, RespBody} ->
      LogMsg = "~p :Error in request StatusCode: ~p RespHeaders: ~p RespBody ~p",
      lager:error(LogMsg, [?TAG, StatusCode, RespHeaders, RespBody]);
    {error, Error} ->
      LogMsg = "~p : Error sending request with payload ~p - ~p",
      lager:error(LogMsg, [?TAG, Payload, Error])
  end.

headers(Key) ->
  [
    {<<"Authorization">>, Key}, 
    {<<"Content-Type">>, <<"application/json">>}
  ].

options() ->
  [
    {pool, false},
    {body_format, binary},
    with_body
  ].
