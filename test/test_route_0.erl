-module(test_route_0).

-author("bnjm").

-export([
         init/2
        ]).
%% GET
init(#{method := <<"GET">>, bindings := #{service := <<"bla">>}} = Req, State) ->
  Response = cowboy_req:reply(200, #{
                                     <<"content-type">> => <<"application/json">>
                                    }, jsone:encode(#{
                                                      <<"hello">> => <<"world">>
                                                     }), Req),
  {ok, Response, State};

init(#{method := <<"GET">>, bindings := #{service := _}} = Req, _) ->
  reply(404, 
        #{<<"error">> => <<"error">>}, Req);

%% list all
init(#{method := <<"GET">>} = Req, _) ->
  reply(200, [],Req);

%% post
init(#{method := <<"POST">>} = Req, _) ->
  ct:print("req: ~p", [Req]),
  case cowboy_req:read_body(Req) of 
    {ok, <<"{\"already\":\"exists\"}">>, Req1} ->
      reply(409, [], Req1); 
    {ok, Body, Req1} ->
      reply(201, Body, Req1);
    _ ->
      reply(401, [], Req)
  end.

reply(Status, Body, Req) when is_map(Body) or is_list(Body) ->
   Response = cowboy_req:reply(Status, #{
                                     <<"content-type">> => <<"application/json">>
                                    }, jsone:encode(Body), Req),
  {ok, Response, {}};

reply(Status, Body, Req) when is_binary(Body) ->
  Response = cowboy_req:reply(Status, #{
                                        <<"content-type">> => <<"application/json">>
                                       }, Body, Req),
  {ok, Response, {}}.
