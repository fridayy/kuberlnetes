-module(test_route_watch_0).

-export([init/2]).

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

modified(Name) ->
  modified(Name, <<"746752">>).
