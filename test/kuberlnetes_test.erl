-module(kuberlnetes_test).

-include_lib("eunit/include/eunit.hrl").

from_config_test_() ->
    application:ensure_all_started(kuberlnetes),
    {ok, BaseDir} = file:get_cwd(),
    TestKubeConfig = BaseDir ++ "/test/config",
    [
        ?_assertEqual(#{}, kuberlnetes:from_config(#{path => TestKubeConfig}))
    ].
