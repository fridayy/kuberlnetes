-module(kuberlnetes_SUITE).

-author("bnjm").

-compile([export_all]).

-include_lib("stdlib/include/assert.hrl").

-define(SERVER, #{server => kuberlnetes:from_raw(test_url())}).

all() ->  
  [
   get_simple_test, 
   get_404_test,
   get_many_test,
   post_one_test,
   post_exists_test,
   patch_happy_test,
   patch_happy_2_test,
   delete_happy_test
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(cowboy),
  TestRoutes = cowboy_router:compile([
                                      {'_', [
                                             {"/api/v1/namespaces/default/services/[:service]", test_route_0, []}
                                            ]}
                                     ]),
  {ok, _} = cowboy:start_clear(test_listener, [], #{
                                          env => #{dispatch => TestRoutes}
                                         }),
  Config.

end_per_suite(_) ->
  cowboy:stop_listener(test_listener),
  ok.

get_simple_test(_) ->
  R = kuberlnetes:get("/api/v1/namespaces/default/services/bla", ?SERVER),
  ?assertMatch({ok, _}, R).

get_404_test(_) ->
  R = kuberlnetes:get("/api/v1/namespaces/default/services/blubb", ?SERVER),
  ?assertEqual({error, resource_not_found}, R).

get_many_test(_) ->
  R = kuberlnetes:get("/api/v1/namespaces/default/services", ?SERVER),
  ?assertMatch({ok, _}, R).

post_one_test(_) ->
  R = kuberlnetes:post(#{path => "/api/v1/namespaces/default/services", body => #{<<"hello">> => <<"world">>}}, ?SERVER),
  ?assertMatch({ok, _}, R).

post_exists_test(_) ->
  R = kuberlnetes:post(#{path => "/api/v1/namespaces/default/services", body => #{<<"already">> => <<"exists">>}}, ?SERVER),
  ?assertEqual({error, already_exists}, R).

patch_happy_test(_) ->
  R = kuberlnetes:patch(#{path => "/api/v1/namespaces/default/services", body => #{<<"happy">> => <<"path">>}}, ?SERVER),
  ?assertMatch({ok, _}, R).

patch_happy_2_test(_) ->
  R = kuberlnetes:patch(#{path => "/api/v1/namespaces/default/services", body => #{<<"kdsjfkj">> => <<"sd">>}}, ?SERVER),
  ?assertMatch({ok, _}, R).

delete_happy_test(_) ->
  R = kuberlnetes:delete("/api/v1/namespaces/default/services/bla", ?SERVER),
  ?assertMatch({ok, _}, R).


test_url() ->
 "http://127.0.0.1:" ++ erlang:integer_to_list(ranch:get_port(test_listener)).
