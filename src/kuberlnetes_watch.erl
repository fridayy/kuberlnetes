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

%% api
-export([start_link/2]).

%% callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_continue/2,
  handle_info/2,
  terminate/2
]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
          %% the owning process of this watch
          %% if the owner goes down the watch will close
          owner :: pid(),
          %% last resource version as indicated by bookmarks
          last_resource_version :: binary() | undefined,
          server :: kuberlnetes:server()
         }).

%% api

-spec start_link(Owner, Server) -> Result when 
    Owner :: pid(),
    Server :: kuberlnetes:server(),
    Result :: gen_server:start_ret().
start_link(Owner, Server)  ->
  gen_server:start_link(?MODULE, [Owner, Server], []).


%% callbacks

init([Owner, Server]) ->
  _OwnerRef = monitor(process, Owner),
  {ok, #state{
          owner = Owner,
          server = Server
         }, {continue, get_resource_version}}.

handle_continue(get_resource_version, #state{server = Server} = State) ->
  % Path = io_lib:format("/api/v1/namespaces/default/configmaps/~s", ["gekko-bridge-config"]),
  % {ok, #{<<"metadata">> := #{<<"resourceVersion">> := Rv}}} = kuberlnetes:get(Path, #{server => Server}),
  % ?LOG_INFO("read rv:", [Rv]),
  % do not pass an rv to request the latest version
  WatchPath = "/api/v1/namespaces/default/configmaps?fieldSelector=metadata.name%3Dgekko-bridge-config&allowWatchBookmarks&watch=true",
  kuberlnetes:watch_req(WatchPath, #{server => Server}),
  {noreply, State}.


handle_call(Message, _From, State) ->
  ?LOG_WARNING(#{event => "unhandled_call", info => #{"message" => Message}}),
  {reply, ok, State}.


handle_cast(Message, State) ->
  ?LOG_WARNING(#{event => "unhandled_cast", info => #{"message" => Message}}),
  {noreply, State}.


handle_info(Message, State) ->
  ?LOG_WARNING(#{event => "unhandled_info", info => #{"message" => Message}}),
  {noreply, State}.


terminate(Reason, State) ->
  ?LOG_WARNING(#{event => "terminated", info => #{"reason" => Reason, "state" => State}}),
  ok.
