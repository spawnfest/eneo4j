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
    application:ensure_all_started(hackney),
    Config.

end_per_suite(_Config) ->
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
    {ok, _, _} = gen_server:call(Worker, {begin_and_commit_transaction, [Statement]}),
    [{worker_config, WorkerConfig} | Config].

end_per_group(_GroupName, Config) ->
    WorkerConfig = ?config(worker_config, Config),
    {ok, Worker} = eneo4j_worker:start_link(WorkerConfig),
    Statement = eneo4j_worker:build_statement(<<"MATCH (n) DETACH DELETE n">>, #{}),
    gen_server:call(Worker, {begin_and_commit_transaction, [Statement]}),
    ok.

init_per_testcase(when_discovering_not_existing_address_econnrefused_is_returned, Config) ->
    DefaultWorkerConfig = ?config(worker_config, Config),
    WorkerConfig = DefaultWorkerConfig#{url => "http://localhost:7475"},
    [{worker_config, WorkerConfig} | Config];
init_per_testcase(when_adding_node_transaction_is_successfully_committed_without_params, Config) ->
    Query = <<"MATCH (n) WHERE n.name = 'Andy' RETURN n">>,
    Params = #{},
    [{cypher_query, Query}, {query_params, Params} | Config];
init_per_testcase(when_adding_node_transaction_is_successfully_committed_with_params, Config) ->
    Query = <<"MATCH (n) WHERE n.name = $name RETURN n">>,
    Params = #{<<"name">> => <<"Andy">>},
    [{cypher_query, Query}, {query_params, Params} | Config];
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
        when_adding_node_transaction_is_successfully_committed_without_params,
        when_unexpected_message_is_send_it_is_logged
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

% Discovery API
when_discovering_not_existing_address_econnrefused_is_returned(Config) ->
    WorkerConfig = ?config(worker_config, Config),
    {ok, Pid} = eneo4j_worker:start_link(WorkerConfig),
    ?assertEqual(
        {error, econnrefused},
        gen_server:call(Pid, discovery_api)
    ),
    Config.

when_discovering_existing_address_version_and_edition_are_returned(Config) ->
    WorkerConfig = ?config(worker_config, Config),
    {ok, Pid} = eneo4j_worker:start_link(WorkerConfig),
    ?assertMatch(
        {ok, _, #{
            <<"neo4j_edition">> := <<"community">>,
            <<"neo4j_version">> := <<"4.1.1">>
        }},
        gen_server:call(Pid, discovery_api)
    ),
    Config.

% Begin and commit transaction
when_adding_node_transaction_is_successfully_committed_without_params(Config) ->
    when_adding_node_transaction_is_successfully_committed(Config).

when_adding_node_transaction_is_successfully_committed_with_params(Config) ->
    when_adding_node_transaction_is_successfully_committed(Config).

when_adding_node_transaction_is_successfully_committed(Config) ->
    WorkerConfig = ?config(worker_config, Config),
    {ok, Worker} = eneo4j_worker:start_link(WorkerConfig),
    Query = ?config(cypher_query, Config),
    Params = ?config(query_params, Config),

    Statement = eneo4j_worker:build_statement(Query, Params),
    ?assertMatch(
        {ok, _, #{<<"errors">> := []}},
        gen_server:call(Worker, {begin_and_commit_transaction, [Statement]})
    ),
    Config.

when_unexpected_message_is_send_it_is_logged(Config) ->
    WorkerConfig = ?config(worker_config, Config),
    {ok, Worker} = eneo4j_worker:start_link(WorkerConfig),
    Worker ! some_unexpected_message,
    % Wait for message to come
    timer:sleep(100),
    ?assert(is_process_alive(Worker)),
    Config.

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
when_unexpected_cast_is_send_it_is_logged(Config) ->
    WorkerConfig = ?config(worker_config, Config),
    {ok, Worker} = eneo4j_worker:start_link(WorkerConfig),
    gen_server:cast(Worker, "unexpected cast"),
    % Wait for message to come
    timer:sleep(100),

    ?assertEqual("Unexpected cast unexpected cast", ?capturedOutput),
    ?assert(is_process_alive(Worker)),
    Config.

-else.
when_unexpected_cast_is_send_it_is_logged(Config) ->
    WorkerConfig = ?config(worker_config, Config),
    {ok, Worker} = eneo4j_worker:start_link(WorkerConfig),
    gen_server:cast(Worker, "unexpected cast"),
    % Wait for message to come
    timer:sleep(100),
    ?assert(is_process_alive(Worker)),
    Config.

-endif.
-endif.
