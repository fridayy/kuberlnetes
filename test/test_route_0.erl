-module(test_route_0).

-author("bnjm").

-export([
    init/2
]).
%% GET
init(#{method := <<"GET">>, bindings := #{service := <<"bla">>}} = Req, _) ->
    reply(200, service(), Req);
init(#{method := <<"DELETE">>, bindings := #{service := <<"bla">>}} = Req, _) ->
    reply(202, service(), Req);
init(#{method := <<"GET">>, bindings := #{service := _}} = Req, _) ->
    reply(404, #{<<"error">> => <<"error">>}, Req);
init(#{method := <<"DELETE">>, bindings := #{service := _}} = Req, _) ->
    reply(404, #{<<"error">> => <<"error">>}, Req);
%% list all
init(#{method := <<"GET">>} = Req, _) ->
    reply(200, serviceList(), Req);
%% post
init(#{method := <<"POST">>} = Req, _) ->
    case cowboy_req:read_body(Req) of
        {ok, <<"{\"already\":\"exists\"}">>, Req1} ->
            reply(409, [], Req1);
        {ok, Body, Req1} ->
            reply(201, Body, Req1);
        _ ->
            reply(401, [], Req)
    end;
%% patch
init(#{method := <<"PATCH">>} = Req, _) ->
    case cowboy_req:read_body(Req) of
        {ok, <<"{\"happy\":\"path\"}">>, Req1} ->
            reply(200, [], Req1);
        {ok, Body, Req1} ->
            reply(201, Body, Req1);
        _ ->
            reply(401, [], Req)
    end.

reply(Status, Body, Req) when is_map(Body) or is_list(Body) ->
    Response = cowboy_req:reply(
        Status,
        #{
            <<"content-type">> => <<"application/json">>
        },
        jsone:encode(Body),
        Req
    ),
    {ok, Response, {}};
reply(Status, Body, Req) when is_binary(Body) ->
    Response = cowboy_req:reply(
        Status,
        #{
            <<"content-type">> => <<"application/json">>
        },
        Body,
        Req
    ),
    {ok, Response, {}}.

service() ->
    #{
        <<"kind">> => <<"Service">>,
        <<"apiVersion">> => <<"v1">>,
        <<"metadata">> => #{
            <<"name">> => <<"bla">>,
            <<"namespace">> => <<"default">>,
            <<"uid">> => <<"e179defd-cf2b-45f1-9d36-04c11d5712e2">>,
            <<"resourceVersion">> => <<"123456">>
        },
        <<"spec">> => #{
            <<"ports">> => [
                #{
                    <<"name">> => <<"http">>,
                    <<"protocol">> => <<"TCP">>,
                    <<"port">> => 80,
                    <<"targetPort">> => <<"http">>
                }
            ],
            <<"selector">> => #{<<"app">> => <<"my-app">>},
            <<"type">> => <<"ClusterIP">>
        },
        <<"status">> => #{<<"loadBalancer">> => #{}}
    }.

serviceList() ->
    #{
        <<"kind">> => <<"ServiceList">>,
        <<"apiVersion">> => <<"v1">>,
        <<"items">> => [service()]
    }.
