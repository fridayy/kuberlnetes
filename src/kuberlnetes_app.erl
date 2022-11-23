%%%=============================================================================
%%% @doc kuberlnetes_app
%%% @end
%%%=============================================================================
-module(kuberlnetes_app).

-behaviour(application).

-export([stop/1, start/2]).

start(_, _) ->
    kuberlnetes_sup:start_link().

stop(_) ->
    ok.
