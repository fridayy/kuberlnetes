-module(kuberlnetes_watch_SUITE).

-compile([nowarn_export_all]).
-compile([export_all]).

-include_lib("stdlib/include/assert.hrl").

all() -> 
  [
   receives_expected_events,
   closes_watch_if_owner_dies,
   handles_gone_messages_appropriately
  ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(kuberlnetes),
    {ok, _} = application:ensure_all_started(cowboy),
    TestRoutes = cowboy_router:compile([
        {'_', [
            {"/api/v1/namespaces/default/configmaps", test_route_watch_0, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(?MODULE, [], #{
        env => #{dispatch => TestRoutes}
    }),
    Config.

end_per_suite(_) ->
    cowboy:stop_listener(?MODULE),
    ok.

%% tests
closes_watch_if_owner_dies(_) ->
  OwnerPid = spawn(fun() -> 
                      kuberlnetes:watch(#{
                      server => kuberlnetes:from_raw(test_url()),
                      kind => configmaps,
                      name => "my-config",
                      namespace => "default"
                     }),
                    timer:sleep(5000)
                   end),
  ct:sleep(50),
  ?assertNotEqual([], kuberlnetes:watches()),
  exit(OwnerPid, byebye),
  ct:sleep(100),
  ?assertEqual([], kuberlnetes:watches()).

receives_expected_events(_) ->
  {ok, _Pid} = kuberlnetes:watch(#{
                      server => kuberlnetes:from_raw(test_url()),
                      kind => configmaps,
                      name => "my-config",
                      namespace => "default"
                     }),
  R = await(3),
  ?assertMatch([
                {kuberlnetes_watch, added, _},
                {kuberlnetes_watch, modified, _},
                {kuberlnetes_watch, modified, #{<<"metadata">> := #{
                                                                    <<"resourceVersion">> := <<"654321">>}}}
               ], R).

handles_gone_messages_appropriately(_) ->
  {ok, _Pid} = kuberlnetes:watch(#{
                      server => kuberlnetes:from_raw(test_url()),
                      kind => configmaps,
                      name => "gone",
                      namespace => "default"
                     }),
  R = await(2),
  ?assertMatch([
                {kuberlnetes_watch, added, _},
                {kuberlnetes_watch, modified, #{<<"metadata">> := #{
                                                                    <<"resourceVersion">> := <<"999999">>}}}
               ], R).

%% support
await(N) ->
  do_await(N, []).

do_await(0, Acc) -> lists:reverse(Acc);
do_await(N, Acc) ->
  receive
    M -> do_await(N - 1, [M | Acc])
  after 1000 -> timeout
  end.


test_url() ->
    "http://127.0.0.1:" ++ erlang:integer_to_list(ranch:get_port(?MODULE)).
