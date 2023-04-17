%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2023, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2023
%%%-------------------------------------------------------------------
-module(kuberlnetes_watch).

-author("bnjm").

-behaviour(gen_server).

-include("kuberlnetes.hrl").

%% api
-export([start_link/3, stop/1]).

%% callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2,
    terminate/2
]).

-type supported_kind() :: configmap.

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_CONNECTION_TIMEOUT, 5000).

-record(state, {
    %% the owning process of this watch
    %% if the owner goes down the watch will close
    owner :: pid(),
    owner_ref :: reference(),
    %% gun connection process
    conn_pid :: pid(),
    %% timer used to indicate and inital connection timeout
    conn_timeout_timer_ref :: reference() | undefined,
    %% last resource version as indicated by bookmarks
    last_resource_version :: binary() | undefined,
    server :: kuberlnetes:server(),
    kind :: supported_kind(),
    namespace :: string(),
    resource_name :: string()
}).

%% api

-spec start_link(Owner, Server, Opts) -> Result when
    Owner :: pid(),
    Server :: kuberlnetes:server(),
    Opts :: map(),
    Result :: gen_server:start_ret().
start_link(Owner, Server, Opts) ->
    gen_server:start_link(?MODULE, [Owner, Server, Opts], []).

-spec stop(Pid) -> Result when
      Pid :: pid(),
      Result :: ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% callbacks

init([Owner, Server, #{kind := Kind, namespace := Namespace, name := ResourceName}]) ->
    %% monitoring the owner allows us to stop
    %% the watch if the owning process goes down
    OwnerRef = monitor(process, Owner),
    % OwnerRef = make_ref(),
    {ok,
        #state{
            owner = Owner,
            owner_ref = OwnerRef,
            server = Server,
            kind = Kind,
            namespace = Namespace,
            resource_name = ResourceName
        }, {continue, open_connection}}.

handle_continue(open_connection, #state{server = Server} = State) ->
    open_connection(Server),
    TimerRef = erlang:send_after(?DEFAULT_CONNECTION_TIMEOUT, self(), connection_timeout),
    %% once the connection is open a gun_up is received otherwise the timer hits
    {noreply, State#state{conn_timeout_timer_ref = TimerRef}}.

handle_info({gun_up, ConnPid, http}, #state{conn_timeout_timer_ref = TimerRef} = State) ->
    maybe_cancel_timer(TimerRef),
    %% if the gun connection process goes down
    %% simply let the watch crash - maybe monitor would be better here?
    erlang:link(ConnPid),
    erlang:process_flag(trap_exit, true),
    watch_req(ConnPid, State),
    {noreply, State#state{conn_pid = ConnPid}};
handle_info(connection_timeout, State) ->
    %% inform owner and stop normally
    State#state.owner ! {kuberlnetes_watch, {error, connection_timeout}},
    {stop, normal, State};
handle_info({gun_data, _ConnPid, _Ref, nofin, Data}, State) ->
    M = jsone:decode(Data),
    NewState = handle_data(M, State),
    {noreply, NewState};
handle_info({gun_data, ConnPid, _Ref, fin, _}, #state{last_resource_version = ResourceVersion} = State) ->
    %% chunked transfer ended - restart from latest bookmark
    ?LOG_WARNING(#{event => "kuberlnetes_watch_fin", info => #{
                                                               "message" => "api server sent FIN - restarting from bookmark",
                                                               "bookmark" => ResourceVersion
                                                              }}),
    watch_req(ConnPid, State),
    {noreply, State};
handle_info({gun_response, _ConnPid, _Ref, nofin, 200, Headers}, State) ->
    case proplists:get_value(<<"transfer-encoding">>, Headers) of
         <<"chunked">> -> {noreply, State};
         _ -> State#state.owner ! {kuberlnetes_watch, {error, not_chunked}},
              {stop, normal, State}
    end;
handle_info({gun_response, _ConnPid, _Ref, _, Status, _}, State) ->
    ?LOG_ERROR(#{error => "kuberlnetes_watch_invalid_resp", info => 
                 #{"message" => "Unexpected response from API server",
                  "status" => Status}}),
    State#state.owner !{kuberlnetes_watch, {error, invalid_response}},
    {stop, normal, State};
handle_info({gun_down, _ConnPif, http, closed, _}, State) ->
    %% the api server apperently closed the connection
    %% terminate and restart
    ?LOG_WARNING(#{event => "kuberlnetes_watch_close", info => 
                   #{"reason" => "server_closed_connection"}}),
    {stop, connection_closed, State};
handle_info({'DOWN', _Ref, process, Owner, _Reason}, #state{owner = Owner} = State) ->
    ?LOG_WARNING(#{event => "kuberlnetes_watch_owner_died", 
                   info => #{"pid" => Owner, "action" => "closing"}}),
    {stop, normal, State};
handle_info(Message, State) ->
    ?LOG_WARNING(#{event => "unhandled_info", info => #{"message" => Message}}),
    {noreply, State}.

handle_call(Message, _From, State) ->
    ?LOG_WARNING(#{event => "unhandled_call", info => #{"message" => Message}}),
    {reply, ok, State}.

handle_cast(Message, State) ->
    ?LOG_WARNING(#{event => "unhandled_cast", info => #{"message" => Message}}),
    {noreply, State}.

terminate(Reason, #state{conn_pid = ConnPid, owner_ref = OwnerRef}) ->
    ?LOG_WARNING(#{event => "terminated", info => #{"reason" => Reason}}),
    gun:close(ConnPid),
    erlang:demonitor(OwnerRef),
    ok.

%% private parts

handle_data(#{<<"type">> := <<"BOOKMARK">>, <<"object">> := Obj}, State) ->
    ?LOG_DEBUG(#{event => "kuberlnetes_watch_bookmark_recv"}),
    #{<<"metadata">> := #{<<"resourceVersion">> := Rv}} = Obj,
    State#state{last_resource_version = Rv};
handle_data(#{<<"type">> := T} = Data, #state{owner = Owner} = State) ->
    TypeAtom = to_atom_type(T),
    #{<<"object">> := O} = Data,
    Owner ! {kuberlnetes_watch, TypeAtom, O},
    State;
handle_data(_, _) ->
    error(invalid_response).

to_atom_type(<<"ADDED">>) -> added;
to_atom_type(<<"MODIFIED">>) -> modified;
to_atom_type(<<"DELETED">>) -> deleted;
to_atom_type(<<"ERROR">>) -> error.

open_connection(Server) ->
    ?LOG_DEBUG(#{event => "connecting", info => #{"host" => Server#server.host, "port" => Server#server.port}}),
    {ok, ConnPid} = gun:open(Server#server.host, Server#server.port, #{
        % transport => tls,
        protocols => [http],
        http_opts => #{ keepalive => 30000},
        tls_opts => [
            {verify, verify_none},
            {cacerts, [Server#server.ca_cert]}
        ]
    }),
    ConnPid.
watch_req(ConnPid, #state{last_resource_version = undefined} = State) -> 
    WatchPath = io_lib:format("/api/v1/namespaces/~s/~s?fieldSelector=metadata.name%3D~s&allowWatchBookmarks&watch=true", 
                              [
                               State#state.namespace,
                               State#state.kind,
                               State#state.resource_name
                          ]),
    gun:get(ConnPid, WatchPath, kuberlnetes:headers(State#state.server, binary));
watch_req(ConnPid, #state{last_resource_version = Rv} = State) -> 
    WatchPath = io_lib:format("/api/v1/namespaces/~s/~s?fieldSelector=metadata.name%3D~s&resourceVersion=~s&allowWatchBookmarks&watch=true", 
                              [
                               State#state.namespace,
                               State#state.kind,
                               State#state.resource_name,
                               Rv
                          ]),
    gun:get(ConnPid, WatchPath, kuberlnetes:headers(State#state.server, binary)).

maybe_cancel_timer(undefined) -> false;
maybe_cancel_timer(TimerRef) when is_reference(TimerRef) ->
    erlang:cancel_timer(TimerRef);
maybe_cancel_timer(_) ->
    error(unexpeced_timer_ref).
