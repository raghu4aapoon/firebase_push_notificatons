-module(fcm_utils).

-export([encode/1, decode/1]).

%% @doc this function encodes the provided data
-spec encode(jsx:json_term()) -> jsx:json_text().
encode(Data) ->
  jsx:encode(Data, [uescape]).

%% @doc this function decodes the provided data and return maps
-spec decode(jsx:json_text()) -> jsx:json_term().
decode(Data) ->
  try
    jsx:decode(Data, [return_maps])
  catch
    _:_ -> throw({bad_json, Data})
  end.