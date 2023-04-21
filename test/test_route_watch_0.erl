-module(test_route_watch_0).

-export([init/2]).

-define(PT_GONE_CALL_COUNT, {?MODULE, store_module}).
-define(GONE_CALL_COUNT, (persistent_term:get(?PT_GONE_CALL_COUNT, 0))).

init(Req, State) ->
  Qs = proplists:to_map(cowboy_req:parse_qs(Req)),
  handle_req(Req, State, Qs).
 
handle_req(Req, State, #{<<"fieldSelector">> := <<"metadata.name=my-config">>,
                         <<"resourceVersion">> := <<"123456">>
                        }) ->
    Req0 = cowboy_req:stream_reply(200, #{
                                        <<"content-type">> => <<"application/json">>
                                       }, Req),
    cowboy_req:stream_body(modified(<<"my-config">>, <<"654321">>), nofin, Req0),
    % prevent 'fin' response 
    timer:sleep(10000),
    {ok, Req0, State};

handle_req(Req, State, #{<<"fieldSelector">> := <<"metadata.name=my-config">>}) ->
   Req0 = cowboy_req:stream_reply(200, #{
                                        <<"content-type">> => <<"application/json">>
                                       }, Req),
  cowboy_req:stream_body(added(<<"my-config">>), nofin, Req0),
  timer:sleep(50),
  cowboy_req:stream_body(modified(<<"my-config">>), nofin, Req0),
  timer:sleep(50),
  cowboy_req:stream_body(bookmark(<<"123456">>), nofin, Req0),
  timer:sleep(50),
  cowboy_req:stream_body(<<>>, fin, Req0),
  {ok, Req0, State};

%% gone test
handle_req(Req, State, #{<<"fieldSelector">> := <<"metadata.name=gone">>,
                         <<"resourceVersion">> := <<"888888">>
                        }) ->
  Req0 = cowboy_req:stream_reply(200, #{
                                        <<"content-type">> => <<"application/json">>
                                       }, Req),
  cowboy_req:stream_body(gone(), fin, Req0),
  {ok, Req0, State};

handle_req(Req, State, #{<<"fieldSelector">> := <<"metadata.name=gone">>}) ->
  Req0 = cowboy_req:stream_reply(200, #{
                                        <<"content-type">> => <<"application/json">>
                                       }, Req),
  case ?GONE_CALL_COUNT of
    0 ->
      persistent_term:put(?PT_GONE_CALL_COUNT, 1),
      cowboy_req:stream_body(added(<<"gone">>), nofin, Req0),
      timer:sleep(50),
      cowboy_req:stream_body(bookmark(<<"888888">>), nofin, Req0),
      timer:sleep(50),
      cowboy_req:stream_body(<<>>, fin, Req0);
    _ ->
      cowboy_req:stream_body(modified(<<"gone">>, <<"999999">>), nofin, Req0),
      timer:sleep(10000)
  end,
  {ok, Req0, State}.

%% responses
bookmark(ResourceVersion) ->
    B = jsone:encode(#{<<"object">> =>
      #{<<"apiVersion">> => <<"v1">>,
        <<"kind">> => <<"ConfigMap">>,
        <<"metadata">> =>
            #{<<"creationTimestamp">> => null,
              <<"resourceVersion">> => ResourceVersion}},
  <<"type">> => <<"BOOKMARK">>}),
  <<B/binary, "\r\n">>.

added(Name) ->
  B = jsone:encode(#{<<"type">> => <<"ADDED">>,
  <<"object">> => 
    #{<<"apiVersion">> => <<"v1">>,
      <<"kind">> => <<"ConfigMap">>,
      <<"metadata">> => 
        #{<<"name">> => Name,
          <<"namespace">> => <<"default">>,
          <<"resourceVersion">> => <<"746850">>},
      <<"data">> => 
        #{<<"key1">> => <<"new_value1">>,
          <<"key2">> => <<"new_value2">>,
          <<"key3">> => <<"value3">>}}}
   ),
  <<B/binary, "\r\n">>.

modified(Name, ResourceVersion) ->
  B = jsone:encode(
    #{<<"type">> => <<"MODIFIED">>,
  <<"object">> => 
    #{<<"apiVersion">> => <<"v1">>,
      <<"kind">> => <<"ConfigMap">>,
      <<"metadata">> => 
        #{<<"name">> => Name,
          <<"namespace">> => <<"default">>,
          <<"resourceVersion">> => ResourceVersion},
      <<"data">> => 
        #{<<"key1">> => <<"value1">>,
          <<"key2">> => <<"value2">>}}}
   ),
  <<B/binary, "\r\n">>.

gone() ->
  B = jsone:encode(#{<<"type">> => <<"ERROR">>,
  <<"object">> => #{<<"kind">> => <<"Status">>,
                     <<"apiVersion">> => <<"v1">>,
                     <<"metadata">> => #{},
                     <<"status">> => <<"Failure">>,
                     <<"message">> => <<"too old resource version:  888888 (63674501)">>,
                     <<"reason">> => <<"Expired">>,
                     <<"code">> => 410}}),
  <<B/binary, "\r\n">>.

modified(Name) ->
  modified(Name, <<"746752">>).
