%%%-------------------------------------------------------------------
%%% @author bnjm
%%% @copyright (C) 2023, leftshift.one software gmbh
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2023
%%%-------------------------------------------------------------------
-module(kuberlnetes_watch_sup).

-author("bnjm").

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Child = #{
        id => kuberlnetes_watch,
        start => {kuberlnetes_watch, start_link, []},
        restart => transient,
        type => worker
    },
    {ok,
        {
            #{
                strategy => simple_one_for_one,
                intesity => 3,
                period => 10
            },
            [Child]
        }}.
