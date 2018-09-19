-module(fcm_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT
-export([all/0, init_per_SUITE/1, end_per_SUITE/1]).

%% Test Cases
-export([push/1]).

-type config() :: proplists:proplist().

-export_type([config/0]).

%%%===================================================================
%%% CT
%%%===================================================================

-spec all() -> [atom()].
all() -> [push].

-spec init_per_SUITE(config()) -> ok.
init_per_SUITE(_Config) ->
  ok = fcm_app:start(),
  _ = application:ensure_all_started(meck),
  ok = mock_firebase_requests().

-spec end_per_SUITE(config()) -> ok.
end_per_SUITE(_Config) ->
  _ = meck:unload(hackney),
  ok = application:stop().

%%%===================================================================
%%% Test Cases
%%%===================================================================

-spec push(pwt_ct:config()) -> ok.
push(_Config) ->

  %% success request
  ok = fcm:push(Payload),

  %% Missing Token in payload
  ok = fcm:push(notification()),

  %% bad request
  ok = fcm:push(#{}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
notification() ->
  #{
    <<"notification">> => #{
      <<"body">> => <<"hello world">>,
      <<"title">> => <<"test message">>
    }
  }.

mock_firebase_requests() ->
  Fun =
    fun(_Url, Headers, Payload, _Options) ->
      response(Headers, fcm_utils:decode(Payload))
    end,
  meck:expect(hackney, post, Fun).

response(_Headers, Payload) when Payload == #{} ->
  {error, <<"bad_request">>};
response(Headers, Payload) ->
  case maps:get(<<"to">>, Payload, <<>>) of
    <<>> ->
      {ok, 400, Headers, fcm_utils:encode(Payload)};
    _ ->
      {ok, 200, Headers, fcm_utils:encode(Payload)}
  end.
