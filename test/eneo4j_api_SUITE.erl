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
        discovery_api_returns_correct_map,
        begin_and_commit_transaction,
        when_nodes_are_create_they_are_can_be_queried
    ].

discovery_api_returns_correct_map(_Config) ->
    ?assertMatch(
        {ok, _, #{
            <<"neo4j_edition">> := <<"community">>,
            <<"neo4j_version">> := <<"4.1.1">>
        }},
        eneo4j:discvery_api()
    ).

begin_and_commit_transaction(_Config) ->
    Query1 = <<"MATCH (n) RETURN n">>,
    Statement1 = eneo4j:build_statement(Query1, #{}),
    Query2 = <<"MATCH (n) WHERE n.name = $name RETURN n">>,
    Params2 = #{<<"name">> => <<"Andy">>},
    Statement2 = eneo4j:build_statement(Query2, Params2, true),
    ?assertMatch(
        {ok, _, #{<<"errors">> := []}},
        eneo4j:begin_and_commit_transaction([Statement1, Statement2])
    ).

when_nodes_are_create_they_are_can_be_queried(_Config) ->
    % Write your query using cypher:
    Query = <<"CREATE (n:Person { name: $name, title: $title });">>,

    % Provide params if needed:
    ParamsAndy = #{
        <<"name">> => <<"Andy">>,
        <<"title">> => <<"Developer">>
    },

    % Build a statement:
    Statement = eneo4j:build_statement(Query, ParamsAndy),

    %Let's build another user:
    ParamsJohn = #{
        <<"name">> => <<"Andy">>,
        <<"title">> => <<"Manager">>
    },

    % We will reuse query, but you may provide a different one if you ant to.
    Statement2 = eneo4j:build_statement(Query, ParamsJohn, true),

    % Lets execute those queries:
    {ok, 200, #{<<"errors">> := []}} = eneo4j:begin_and_commit_transaction([Statement, Statement2]),

    %Lets now try getting Persons names
    QueryGetPersonsNames = <<"MATCH (n:Person) RETURN n.name">>,
    Statement3 = eneo4j:build_statement(QueryGetPersonsNames, #{}),
    {ok, 200, #{<<"errors">> := [], <<"results">> := _}} = eneo4j:begin_and_commit_transaction([
        Statement3
    ]).
