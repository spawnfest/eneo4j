-module(eneo4j_benchmark_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(NUMBER_OF_PARALLEL_EXECUTIONS, 100).
-define(AuthNeo4jUrl, "http://localhost:7474").
-define(NoAuthNeo4jUrl, "http://localhost:7470").
-define(Neo4jUser, "neo4j").
-define(Neo4jDB, "neo4j").
-define(Neo4jPassword, "test").

suite() -> [{timetrap, {minutes, 10}}].

init_per_suite(Config) ->
    % Change skip to the confing to run this benchamrk
    % then run benchmark with rebar3 ct --suite eneo4j_benchmark_SUITE
    % [{insert_nodes, 10} | Config].
    {skip, "Benchmark should not be executed on normal CI"}.

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
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [
        {no_authentication, [parallel, shuffle], [
            connect_5_nearest
            | lists:duplicate(?NUMBER_OF_PARALLEL_EXECUTIONS, insert_n_nodes)
        ]},
        {authentication, [parallel, shuffle], [
            connect_5_nearest
            | lists:duplicate(?NUMBER_OF_PARALLEL_EXECUTIONS, insert_n_nodes)
        ]}
    ].

all() ->
    [
        {group, no_authentication},
        {group, authentication}
    ].

insert_n_nodes() -> [{timetrap, {seconds, 60}}].

insert_n_nodes(Config) ->
    InsertNodes = ?config(insert_nodes, Config),
    Statements = [
        begin
            Query = <<"CREATE (n:Person { name: $name })">>,
            Params = #{<<"name">> => random_string()},
            eneo4j:build_statement(Query, Params)
        end
        || _ <- lists:seq(1, InsertNodes)
    ],

    {Time, _Val} = timer:tc(fun() ->
        eneo4j:begin_and_commit_transaction(Statements)
    end),

    ct:log("Insert ~p nodes in ~p", [InsertNodes, Time]).

connect_5_nearest(_Config) ->
    Query =
        <<"MATCH (a:Person),(b:Person)\n"
            "        WHERE id(a) + 5 > id(b) AND id(a) - 5 < id(b)\n"
            "        CREATE (a)-[r:Knows]->(b)\n"
            "        RETURN type(r)">>,
    Statement = eneo4j:build_statement(Query, #{}),
    {Time, _Val} = timer:tc(fun() ->
        eneo4j:begin_and_commit_transaction([Statement])
    end),

    ct:log("Connect nodes time ~p", [Time]).

random_string() ->
    Bits = entropy_string:bits(1.0e6, 1.0e9),
    entropy_string:random_string(Bits).
