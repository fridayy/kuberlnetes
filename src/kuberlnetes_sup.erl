%%%=============================================================================
%%% @doc kuberlnetes_sup
%%% @end
%%%=============================================================================
-module(kuberlnetes_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
                  #{
                    id => kuberlnetes_watch_sup,
                    start => {kuberlnetes_watch_sup, start_link, []},
                    type => supervisor
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.
