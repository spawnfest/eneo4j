-module(eneo4j_worker_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(AuthNeo4jUrl, "http://localhost:7474").
-define(NoAuthNeo4jUrl, "http://localhost:7470").
-define(Neo4jUser, "neo4j").
-define(Neo4jPassword, "test").

init_per_suite(Config) ->
    application:ensure_all_started(eneo4j),
    Config.

end_per_suite(_Config) ->
    application:stop(eneo4j),
    ok.

init_per_group(no_authentication, Config) ->
    [{worker_config, #{url => ?NoAuthNeo4jUrl}} | Config];
init_per_group(with_authentication, Config) ->
    [
        {worker_config, #{url => ?AuthNeo4jUrl, user => ?Neo4jUser, password => ?Neo4jPassword}}
        | Config
    ].

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(when_discovering_not_existing_address_econnrefused_is_returned, Config) ->
    DefaultWorkerConfig = proplists:get_value(worker_config, Config),
    WorkerConfig = DefaultWorkerConfig#{url => "http://localhost:7475"},
    [{worker_config, WorkerConfig} | Config];
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

groups() ->
    [
        {no_authentication, [], common_test_cases()},
        {with_authentication, [], common_test_cases()}
    ].

all() ->
    [
        {group, no_authentication},
        {group, with_authentication}
    ].

common_test_cases() ->
    [
        when_discovering_not_existing_address_econnrefused_is_returned,
        when_discovering_existing_address_version_and_edition_are_returned
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

% Discovery API
when_discovering_not_existing_address_econnrefused_is_returned(Config) ->
    WorkerConfig = proplists:get_value(worker_config, Config),
    {ok, Pid} = eneo4j_worker:start_link(WorkerConfig),
    ?assertEqual(
        {error, econnrefused},
        gen_server:call(Pid, discovery_api)
    ).

when_discovering_existing_address_version_and_edition_are_returned(Config) ->
    WorkerConfig = proplists:get_value(worker_config, Config),
    {ok, Pid} = eneo4j_worker:start_link(WorkerConfig),
    ?_assertMatch(
        #{
            <<"neo4j_edition">> := <<"community">>,
            <<"neo4j_version">> := <<"4.1.1">>
        },
        gen_server:call(Pid, discovery_api)
    ).
