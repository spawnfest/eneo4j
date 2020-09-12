-module(eneo4j_api_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(AuthNeo4jUrl, "http://localhost:7474").
-define(NoAuthNeo4jUrl, "http://localhost:7470").
-define(Neo4jUser, "neo4j").
-define(Neo4jDB, "neo4j").
-define(Neo4jPassword, "test").

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(no_authentication, Config) ->
    Eneo4jWorkerConfig = #{
        url => ?NoAuthNeo4jUrl,
        db => ?Neo4jDB
    },
    persistent_term:put(eneo4j_worker_config, Eneo4jWorkerConfig),
    persistent_term:put(eneo4j_workers_count, 5),
    {ok, _} = application:ensure_all_started(eneo4j),
    Config;
init_per_group(authentication, Config) ->
    Eneo4jWorkerConfig = #{
        url => ?AuthNeo4jUrl,
        db => ?Neo4jDB,
        user => ?Neo4jUser,
        password => ?Neo4jPassword
    },
    persistent_term:put(eneo4j_worker_config, Eneo4jWorkerConfig),
    persistent_term:put(eneo4j_workers_count, 5),
    {ok, _} = application:ensure_all_started(eneo4j),
    Config.

end_per_group(_GroupName, _Config) ->
    application:stop(eneo4j),
    ok.

init_per_testcase(_Case, Config) -> Config.

end_per_testcase(_Case, _Config) -> ok.

all() ->
    [
        {group, no_authentication},
        {group, authentication}
    ].

groups() ->
    [
        {no_authentication, [], tests()},
        {authentication, [], tests()}
    ].

tests() ->
    [
        discovery_api_returns_correct_map
    ].

discovery_api_returns_correct_map(_Config) ->
    ?assertMatch(
        {ok, _, #{
            <<"neo4j_edition">> := <<"community">>,
            <<"neo4j_version">> := <<"4.1.1">>
        }},
        eneo4j:discvery_api()
    ).
