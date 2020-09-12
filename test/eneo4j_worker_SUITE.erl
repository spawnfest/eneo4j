-module(eneo4j_worker_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(AuthNeo4jUrl, "http://localhost:7474").
-define(NoAuthNeo4jUrl, "http://localhost:7470").
-define(Neo4jUser, "neo4j").
-define(Neo4jDB, "neo4j").
-define(Neo4jPassword, "test").

init_per_suite(Config) ->
    application:ensure_all_started(eneo4j),
    Config.

end_per_suite(_Config) ->
    application:stop(eneo4j),
    ok.

init_per_group(no_authentication, Config) ->
    WorkerConfig = #{url => ?NoAuthNeo4jUrl, db => ?Neo4jDB},
    {ok, Worker} = eneo4j_worker:start_link(WorkerConfig),
    Statement = eneo4j_worker:build_statement(
        <<"CREATE (n:Person { name: 'Andy' }) RETURN n.name">>,
        #{}
    ),
    gen_server:call(Worker, {begin_and_commit_transaction, [Statement]}),
    [{worker, Worker}, {worker_config, WorkerConfig} | Config];
init_per_group(with_authentication, Config) ->
    WorkerConfig = #{
        url => ?AuthNeo4jUrl,
        user => ?Neo4jUser,
        password => ?Neo4jPassword,
        db => ?Neo4jDB
    },
    {ok, Worker} = eneo4j_worker:start_link(WorkerConfig),
    Statement = eneo4j_worker:build_statement(
        <<"CREATE (n:Person { name: 'Andy' }) RETURN n.name">>,
        #{}
    ),
    gen_server:call(Worker, {begin_and_commit_transaction, [Statement]}),
    [{worker, Worker}, {worker_config, WorkerConfig} | Config].

end_per_group(_GroupName, Config) ->
    WorkerConfig = proplists:get_value(worker_config, Config),
    {ok, Worker} = eneo4j_worker:start_link(WorkerConfig),
    Statement = eneo4j_worker:build_statement(<<"MATCH (n) DETACH DELETE n">>, #{}),
    gen_server:call(Worker, {begin_and_commit_transaction, [Statement]}),
    ok.

init_per_testcase(when_discovering_not_existing_address_econnrefused_is_returned, Config) ->
    DefaultWorkerConfig = proplists:get_value(worker_config, Config),
    WorkerConfig = DefaultWorkerConfig#{url => "http://localhost:7475"},
    [{worker_config, WorkerConfig} | Config];
init_per_testcase(when_adding_node_transaction_is_successfully_committed_without_params, Config) ->
    Query = <<"MATCH (n) WHERE n.name = 'Andy' RETURN n">>,
    Params = #{},
    [{cypher_query, Query}, {Params} | Config];
init_per_testcase(when_adding_node_transaction_is_successfully_committed_with_params, Config) ->
    Query = <<"MATCH (n) WHERE n.name = $name RETURN n">>,
    Params = #{<<"name">> => <<"Andy">>},
    [{cypher_query, Query}, {Params} | Config];
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

groups() ->
    [
        {no_authentication, [parallel], common_test_cases()},
        {with_authentication, [parallel], common_test_cases()}
    ].

all() ->
    [
        {group, no_authentication},
        {group, with_authentication}
    ].

common_test_cases() ->
    [
        when_discovering_not_existing_address_econnrefused_is_returned,
        when_discovering_existing_address_version_and_edition_are_returned,
        when_adding_node_transaction_is_successfully_committed_with_params,
        when_adding_node_transaction_is_successfully_committed_without_params
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
        {ok, _, #{
            <<"neo4j_edition">> := <<"community">>,
            <<"neo4j_version">> := <<"4.1.1">>
        }},
        gen_server:call(Pid, discovery_api)
    ).

% Begin and commit transaction
when_adding_node_transaction_is_successfully_committed_without_params(Config) ->
    when_adding_node_transaction_is_successfully_committed(Config).

when_adding_node_transaction_is_successfully_committed_with_params(Config) ->
    when_adding_node_transaction_is_successfully_committed(Config).

when_adding_node_transaction_is_successfully_committed(Config) ->
    Worker = proplists:get_value(worker, Config),
    Query = proplists:get_value(cypher_query, Config),
    Params = proplists:get_value(query_params, Config),

    Statement = eneo4j_worker:build_statement(Query, Params),
    ?_assertMatch(
        {ok, _, #{}},
        gen_server:call(Worker, {begin_and_commit_transaction, [Statement]})
    ).
