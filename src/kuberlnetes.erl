%%%=============================================================================
%%% @doc kuberlnetes
%%% A low level kubernetes api helper - primarlily for applications that interact
%%% with the kubernetes api from inside the cluster.
%%% @end
%%%=============================================================================
-module(kuberlnetes).

-export([in_cluster/0, get/2, post/2, patch/2, microtime_now/0]).
-export([from_config/0]).

-author("bnjm").

-type options() :: #{
    server => server() | undefined
}.

-type kube_cfg_options() :: #{
                              %% path to the kube config file
                              %% defaults to user.home/.kube/config
                              path => string(),
                              %% the context to be selected
                              %% defaults to "current-context"
                              context => string() 
                             }.

-type mutation_request() :: #{
    path => string(),
    body => map()
}.


-record(auth_token, {token :: binary()}).
-record(server, {
    url :: string(),
    ca_cert :: string(),
    auth :: #auth_token{},
    skip_tls_verify = false
}).

-type server() :: #server{}.

%% @doc
%% Configures a server using the service account
%% @end
-spec in_cluster() -> server().
in_cluster() ->
    Host = os:getenv("KUBERNETES_SERVICE_HOST"),
    Port = os:getenv("KUBERNETES_SERVICE_PORT"),
    {ok, Token} = file:read_file("/var/run/secrets/kubernetes.io/serviceaccount/token"),
    #server{
        url = "https://" ++ Host ++ ":" ++ Port,
        ca_cert = cert_from_file("/var/run/secrets/kubernetes.io/serviceaccount/ca.crt"),
        auth = #auth_token{token = Token}
    }.

%% @doc
%% Loads the current-context from ~/.kube/config
-spec from_config(Options) -> server() when
      Options :: kube_cfg_options().
from_config(Opts) ->
    KubeConfigPath = maps:get(path, Opts, default_kube_cfg_path()),
    SelectedContext = maps:get(context, Opts, "current-context"),
   [ KubeConfig | _ ] = yamerl_constr:file(KubeConfigPath),
    User = get_relevant_user(SelectedContext, KubeConfig),
    Auth = get_auth(User, KubeConfig),
    Cert = get_certificate(User, KubeConfig),
    Url = get_url(SelectedContext, KubeConfig),
    #server{
       url = Url,
       ca_cert = Cert,
       auth = Auth
      }.

from_config() -> from_config(#{}).

%% @doc
%% Initiates a GET request against the configured kubernetes api server
%% or raises an error if the server is not configured correctly.
%% @end
-spec get(Path, Opts) -> Body when
    Path :: string(),
    Opts :: options(),
    Body :: map().
get(Path, Opts) ->
    Server = get_server(Opts),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(
        get,
        {Server#server.url ++ Path, headers(Server)},
        [{ssl, [{cacerts, [Server#server.ca_cert]}]}],
        []
    ),
    decode(Body).

%% @doc
%% Initiates a POST request against the configured kubernetes api server or
%% raises an error if the server is not configured correctly.
%% see: server()
%% @end
-spec post(Request, Opts) -> ok when
    Request :: mutation_request(),
    Opts :: options().
post(#{path := Path, body := Body}, Opts) ->
    Server = get_server(Opts),
    {ok, {{_, 201, _}, _, _}} = httpc:request(
        post,
        {Server#server.url ++ Path, headers(Server), "application/json", jsone:encode(Body)},
        [{ssl, [{cacerts, [Server#server.ca_cert]}]}],
        []
    ),
    ok.

%% @doc
%% Initiates a PATCH request against the configured kubernetes api server or
%% raises an error if the server is not configured correctly.
%% see: server()
%% @end
-spec patch(Request, Opts) -> ok when
    Request :: mutation_request(),
    Opts :: options().
patch(#{path := Path, body := Body}, Opts) ->
    Server = get_server(Opts),
    {ok, {{_, 200, _}, _, _}} = httpc:request(
        patch,
        {
            Server#server.url ++ Path,
            headers(Server),
            "application/merge-patch+json",
            jsone:encode(Body)
        },
        [{ssl, [{cacerts, [Server#server.ca_cert]}]}],
        []
    ),
    ok.

%% @doc
%% Returns the current datetime in the kubernetes MicroTime format
%% see: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.22/#microtime-v1-meta
%% @end
-spec microtime_now() -> binary().
microtime_now() ->
    erlang:list_to_binary(
        calendar:system_time_to_rfc3339(erlang:system_time(microsecond), [
            {unit, microsecond}, {offset, "Z"}
        ])
    ).

%% internal functions
headers(Server) -> headers(Server, []).
headers(#server{auth = #auth_token{token = Token}}, AdditionalHeaders) ->
    [
        {"Accept", "application/json"},
        {"Authorization", "Bearer " ++ Token}
    ] ++ AdditionalHeaders.

cert_from_file(Path) ->
    true = filelib:is_file(Path),
    {ok, BinaryContent} = file:read_file(Path),
    [{'Certificate', Data, _}] = public_key:pem_decode(BinaryContent),
    Data.

cert_from_b64(CertData) when is_list(CertData) ->
    DecodedCert = base64:decode(CertData),
    [{'Certificate', Data, _}] = public_key:pem_decode(DecodedCert),
    Data.

get_server(Opts) ->
    OptsServer = maps:get(server, Opts, undefined),
    case OptsServer of
        undefined -> error(no_server_configured);
        Else when is_record(Else, server) -> Else;
        _ -> error(invalid_server)
    end.

decode(Body) when is_binary(Body) -> jsone:decode(Body);
decode(Body) when is_list(Body) -> jsone:decode(erlang:list_to_binary(Body)).

get_relevant_user(CurrentContext, KubeConfig) -> 
    RelevantContext = get_context_for_name(CurrentContext, KubeConfig),
    RelevantUser = proplists:get_value("user", proplists:get_value("context", RelevantContext)),
    RelevantUser.

%% todo: remove duplication
get_context_for_name(Name, KubeConfig) when is_list(Name) and is_list(KubeConfig) ->
    AllContexts = proplists:get_value("contexts", KubeConfig),
    [H | _] = lists:filter(fun(Context) -> proplists:get_value("name", Context) =:= Name end, AllContexts),
    H.

find_user(User, KubeConfig) ->
    Users = proplists:get_value("users", KubeConfig),
    [H | _] = lists:filter(fun(X) -> proplists:get_value("name", X) =:= User end, Users),
    proplists:get_value("user", H).

get_cluster_for_name(Name, KubeConfig) when is_list(Name) and is_list(KubeConfig) ->
    AllClusters = proplists:get_value("clusters", KubeConfig),
    [H | _] = lists:filter(fun(Cluster) -> proplists:get_value("name", Cluster) =:= Name end, AllClusters),
    proplists:get_value("cluster", H).

get_certificate(User, KubeConfig) when is_list(User) ->
    U = find_user(User, KubeConfig),
    CertData = proplists:get_value("client-certificate-data", U),
    cert_from_b64(CertData).
%% end todo
%%
get_auth(User, KubeConfig) when is_list(User) and is_list(KubeConfig) ->
    U = find_user(User, KubeConfig),
    do_get_auth(proplists:to_map(U)).

do_get_auth(#{"token" := T}) when is_list(T) -> #auth_token{token = list_to_binary(T)}.

get_url(CurrentContext, KubeConfig) ->
    RelevantCluster = get_cluster_for_name(CurrentContext, KubeConfig),
    proplists:get_value("server", RelevantCluster).

default_kube_cfg_path() ->
    HomeDir = os:getenv("HOME"),
    HomeDir ++ "/.kube/config".
 

