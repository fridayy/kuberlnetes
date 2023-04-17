%%%=============================================================================
%%% @doc kuberlnetes_app
%%% @end
%%%=============================================================================
-module(kuberlnetes_app).

-behaviour(application).

-export([stop/1, start/2]).

-include("kuberlnetes.hrl").

start(_, _) ->
    create_ets_table(),
    kuberlnetes_sup:start_link().

stop(_) ->
    ok.

%% private parts
create_ets_table() ->
    case ets:info(?SERVER_CFG_TAB) of
        undefined ->
            ets:new(?SERVER_CFG_TAB, [set, public, named_table]),
            ok;
        _ ->
            ok
    end.
