-module(kuberlnetes_test).

-include_lib("eunit/include/eunit.hrl").
-include("kuberlnetes.hrl").

from_config_current_context_test_() ->
    application:ensure_all_started(kuberlnetes),
    {ok, BaseDir} = file:get_cwd(),
    TestKubeConfig = BaseDir ++ "/test/config",
    R = kuberlnetes:from_config(#{path => TestKubeConfig}),
    [
        ?_assertEqual("https://localhost:6443", R#server.url),
        ?_assert(is_binary(R#server.ca_cert)),
        ?_assertEqual("test-cluster", R#server.clustername),
        ?_assertEqual("test-user", R#server.username),
        ?_assertNotEqual(undefined, R#server.auth),
        ?_assertMatch(#auth_cert{cert = _, key = _}, R#server.auth)
    ].
