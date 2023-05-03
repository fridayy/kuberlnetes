-module(kuberlnetes_watch_SUITE).

-compile([nowarn_export_all]).
-compile([export_all]).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        receives_expected_events,
        closes_watch_if_owner_dies,
        handles_gone_messages_appropriately,
        can_handle_different_receiver,
        does_not_crash_if_monitoring_off,
        does_crash_if_monitoring_on,
        does_crash_if_gun_conn_crashes,
        handles_split_events
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
            api_group => "/api/v1",
            kind => "configmaps",
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

can_handle_different_receiver(_) ->
    Self = self(),
    OwnerPid = spawn(
        fun() ->
            receive
                M -> Self ! {self(), M}
            after 5000 -> self ! timeout
            end
        end
    ),
    {ok, WatchPid} = kuberlnetes:watch(#{
        server => kuberlnetes:from_raw(test_url()),
        api_group => "/api/v1",
        kind => "configmaps",
        name => "my-config",
        namespace => "default",
        receiver => OwnerPid,
        monitor => false
    }),
    R =
        receive
            {OwnerPid, M} -> M
        after 5000 -> timeout
        end,
    ?assertMatch({kuberlnetes_watch, added, _}, R),
    kuberlnetes:watch_close(WatchPid),
    ?assertNot(is_process_alive(WatchPid)).

does_not_crash_if_monitoring_off(_) ->
    OwnerPid = spawn(
        fun() ->
            timer:sleep(9999)
        end
    ),
    {ok, WatchPid} = kuberlnetes:watch(#{
        server => kuberlnetes:from_raw(test_url()),
        api_group => "/api/v1",
        kind => "configmaps",
        name => "my-config",
        namespace => "default",
        receiver => OwnerPid,
        monitor => false
    }),
    exit(OwnerPid, byebye),
    ct:sleep(20),
    ?assertNot(is_process_alive(OwnerPid)),
    ?assert(is_process_alive(WatchPid)),
    kuberlnetes:watch_close(WatchPid),
    ?assertNot(is_process_alive(WatchPid)).

does_crash_if_monitoring_on(_) ->
    OwnerPid = spawn(
        fun() ->
            timer:sleep(9999)
        end
    ),
    {ok, WatchPid} = kuberlnetes:watch(#{
        server => kuberlnetes:from_raw(test_url()),
        api_group => "/api/v1",
        kind => "configmaps",
        name => "my-config",
        namespace => "default",
        receiver => OwnerPid,
        monitor => true
    }),
    exit(OwnerPid, byebye),
    ct:sleep(20),
    ?assertNot(is_process_alive(OwnerPid)),
    ?assertNot(is_process_alive(WatchPid)).

does_crash_if_gun_conn_crashes(_) ->
    {ok, WatchPid} = kuberlnetes:watch(#{
        server => kuberlnetes:from_raw(test_url()),
        api_group => "/api/v1",
        kind => "configmaps",
        name => "my-config",
        namespace => "default",
        monitor => false
    }),
    ct:sleep(100),
    [{_, ConnPid, _, _} | []] = supervisor:which_children(gun_conns_sup),
    exit(ConnPid, die),
    ct:sleep(20),
    ?assertNot(is_process_alive(ConnPid)),
    ?assertNot(is_process_alive(WatchPid)).

receives_expected_events(_) ->
    {ok, WatchPid} = kuberlnetes:watch(#{
        server => kuberlnetes:from_raw(test_url()),
        api_group => "/api/v1",
        kind => "configmaps",
        name => "my-config",
        namespace => "default"
    }),
    R = await(3),
    ?assertMatch(
        [
            {kuberlnetes_watch, added, _},
            {kuberlnetes_watch, modified, _},
            {kuberlnetes_watch, modified, #{
                <<"metadata">> := #{
                    <<"resourceVersion">> := <<"654321">>
                }
            }}
        ],
        R
    ),
    kuberlnetes:watch_close(WatchPid),
    ?assertNot(is_process_alive(WatchPid)).

handles_gone_messages_appropriately(_) ->
    {ok, WatchPid} = kuberlnetes:watch(#{
        server => kuberlnetes:from_raw(test_url()),
        api_group => "/api/v1",
        kind => "configmaps",
        name => "gone",
        namespace => "default"
    }),
    R = await(2),
    ?assertMatch(
        [
            {kuberlnetes_watch, added, _},
            {kuberlnetes_watch, modified, #{
                <<"metadata">> := #{
                    <<"resourceVersion">> := <<"999999">>
                }
            }}
        ],
        R
    ),
    kuberlnetes:watch_close(WatchPid),
    ?assertNot(is_process_alive(WatchPid)).

handles_split_events(_) ->
    {ok, WatchPid} = kuberlnetes:watch(#{
        server => kuberlnetes:from_raw(test_url()),
        api_group => "/api/v1",
        kind => "configmaps",
        name => "split",
        namespace => "default"
    }),
    R = await(1),
    ?assertMatch(
        [
            {kuberlnetes_watch, added, _}
        ],
        R
    ),
    kuberlnetes:watch_close(WatchPid),
    ?assertNot(is_process_alive(WatchPid)).

%% support
await(N) ->
    await(N, []).

await(0, Acc) ->
    lists:reverse(Acc);
await(N, Acc) ->
    receive
        M -> await(N - 1, [M | Acc])
    after 1000 -> timeout
    end.

test_url() ->
    "http://127.0.0.1:" ++ erlang:integer_to_list(ranch:get_port(?MODULE)).
