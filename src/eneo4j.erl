%%% @author Aleksander Lisiecki <alek.lisiecki@gmail.com>
%%% @doc
%%% This module provides functions to begin, run, commit and rollback transactions,
%%% and statement builder.
%%% It is a main API for this library.

-module(eneo4j).

-export([
    discovery_api/0,
    begin_and_commit_transaction/1,
    begin_transaction/1,
    run_queries_inside_transaction/2,
    keep_alive_transaction/1,
    commit_transaction/2,
    rollback_transaction/1
]).

-export([
    build_statement/2,
    build_statement/3
]).

-ignore_xref([
    {?MODULE, discovery_api, 0},
    {?MODULE, begin_and_commit_transaction, 1},
    {?MODULE, begin_transaction, 1},
    {?MODULE, run_queries_inside_transaction, 2},
    {?MODULE, keep_alive_transaction, 1},
    {?MODULE, commit_transaction, 2},
    {?MODULE, rollback_transaction, 1},
    {?MODULE, build_statement, 2},
    {?MODULE, build_statement, 3}
]).

-export_type([response/0, response_with_commit/0]).

-type discovery_api_response() :: #{binary() := binary()}.
-type response() :: #{binary() := any()}.
-type response_with_commit() :: #{binary() := any()}.
-type query_result() :: {ok, response()} | {error, any()}.
-type query_result_with_commit() :: {ok, response_with_commit()} | {error, any()}.
-type statements() :: eneo4j_worker:statements().
-type statement() :: eneo4j_worker:statement().

%%% @doc
%%% Use this function to extract information about neo4j version, distribution, etc.
%%%
%%% It may be used to check the connection and if the database wa configured correctly.

%%% Following code should work if you have the right url configured:
%%% ```
%%% #{
%%%     <<"neo4j_edition">> := _,
%%%     <<"neo4j_version">> := _
%%% } = eneo4j:discovery_api()
%%% '''
-spec discovery_api() -> discovery_api_response().
discovery_api() ->
    {ok, 200, Response} = wpool:call(eneo4j_workers_pool, discovery_api, available_worker),
    Response.

%%% @doc
%%% Use this function to begin and commit a transaction in a single HTTP request.
%%%
%%% ```
%%% Query = <<"CREATE (n:Person { name: $name, title: $title });">>,
%%%
%%% % Provide params if needed:
%%% ParamsAndy = #{
%%%  <<"name">> => <<"Andy">>,
%%%  <<"title">> => <<"Developer">>
%%%  },
%%%
%%% % Build a statement:
%%% Statement = eneo4j:build_statement(Query, ParamsAndy),
%%% Execute the query
%%% {ok, Result} = eneo4j:begin_and_commit_transaction([Statement]).
%%% '''
-spec begin_and_commit_transaction(statements()) -> query_result().
begin_and_commit_transaction(Statements) ->
    Request = {begin_and_commit_transaction, Statements},
    call_wpool(Request).

%%% @doc
%%% Use this function to begin transaction
%%%
%%% ```
%%% QueryGetPersonsNames = <<"MATCH (n:Person) RETURN n.name">>,
%%% Statement = eneo4j:build_statement(QueryGetPersonsNames, #{}),
%%% {ok, Result} = eneo4j:begin_transaction([Statement]),
%%% '''
-spec begin_transaction(statements()) -> query_result_with_commit().
begin_transaction(Statements) ->
    Request = {begin_transaction, Statements},
    call_wpool(Request).

%%% @doc
%%% Use this function to begin transaction
%%%
%%% ```
%%% {ok, BeginResponse} = eneo4j:begin_transaction([]),
%%%
%%% Query = <<"MATCH (n:Person) RETURN n.name">>,
%%% Statement = eneo4j:build_statement(Query, #{}),
%%%
%%% {ok, RunLink} = eneo4j_response:get_run_queries_link(BeginResponse),
%%% {ok, RunResponse} = eneo4j:run_queries_inside_transaction([Statement], RunLink),
%%%
%%% % Run queries inside transaction many times:
%%% {ok, RunLink2} = eneo4j_response:get_run_queries_link(RunResponse),
%%% {ok, Result} = eneo4j:run_queries_inside_transaction([Statement], RunLink2),
%%% '''
-spec run_queries_inside_transaction(statements(), eneo4j_response:run_queries_link()) ->
    query_result_with_commit().
run_queries_inside_transaction(Statements, RunLink) ->
    Request = {{run_queries, RunLink}, Statements},
    call_wpool(Request).

%%% @doc
%%% Use this function to keep transaction alive.
%%% It will timeout after 60 seconds (by default).
%%%
%%% ```
%%% {ok, BeginResponse} = eneo4j:begin_transaction([]),
%%%
%%% Query = <<"MATCH (n:Person) RETURN n.name">>,
%%% Statement = eneo4j:build_statement(Query, #{}),
%%%
%%% timer:sleep(timer:seconds(40)),
%%%
%%% {ok, RunLink} = eneo4j_response:get_run_queries_link(BeginResponse),
%%% {ok, RunResponse} = eneo4j:keep_alive_transaction(RunLink),
%%%
%%% % Normally transaction would timeout here,
%%% % but thanks to eneo4j:keep_alive_transaction/1 call it will remain opened.
%%%
%%% timer:sleep(timer:seconds(40)),
%%%
%%% % Run queries inside transaction many times:
%%%
%%% {ok, RunLink2} = eneo4j_response:get_run_queries_link(RunResponse),
%%% {ok, Result} = eneo4j:run_queries_inside_transaction([Statement], RunLink2),
%%% '''

-spec keep_alive_transaction(eneo4j_response:run_queries_link()) -> query_result_with_commit().
keep_alive_transaction(RunLink) ->
    run_queries_inside_transaction([], RunLink).

%%% @doc
%%% Use this function commit a transaction.
%%%
%%% ```
%%%  {ok, Response} = eneo4j:begin_transaction([Statement]),
%%%  {ok, CommitLink} = eneo4j_response:get_commit_transaction_link(Response),
%%%
%%%  % You may add statements when committing a transaction
%%%
%%%  Statements = [],
%%%  {ok, Result} = eneo4j:commit_transaction(Statements, CommitLink),
%%% '''
-spec commit_transaction(statements(), eneo4j_response:commit_transaction_link()) ->
    query_result_with_commit().
commit_transaction(Statements, CommitLink) ->
    Request = {{commit_transaction, CommitLink}, Statements},
    call_wpool(Request).

%%% @doc
%%% Use this function rollback a transaction.
%%%
%%% ```
%%%  {ok, Response} = eneo4j:begin_transaction([Statement]),
%%%  {ok, RollbackLink} = eneo4j_response:get_rollback_transaction_link(Response),
%%%
%%%  % You may add statements when committing a transaction
%%%
%%%  Statements = [],
%%%  {ok, Result} = eneo4j:rollback_transaction(Statements, RollbackLink),
%%% '''
-spec rollback_transaction(eneo4j_response:rollback_transaction_link()) ->
    query_result_with_commit().
rollback_transaction(RollbackLink) ->
    Request = {rollback_transaction, RollbackLink},
    call_wpool(Request).

%%% @doc
%%% This function allows to build a statement and include query stats.
%%% @param Query - is a binary form query
%%% @param Params - is a map providing params to a query
%%% @param IncludeStats - is a boolean, when `true' query stats are included in a response
%%%
%%% This function allows to build a statement to pass to be send as a query.
%%% ```
%%% Query = <<"MATCH (n) WHERE n.name = $name RETURN n">>,
%%% Params = #{<<"name">> => <<"Andy">>},
%%% Statement = eneo4j:build_statement(Query, Params, true),
%%% '''
-spec build_statement(binary(), #{binary() => any()}, boolean()) -> statement().
build_statement(Query, Params, IncludeStats) ->
    eneo4j_worker:build_statement(Query, Params, IncludeStats).

%%% @doc
%%% This function works the same as eneo4j:build_statement(Query, Params, false)
%%%
%%% ```
%%% Query = <<"MATCH (n) WHERE n.name = $name RETURN n">>,
%%% Params = #{<<"name">> => <<"Andy">>},
%%% Statement = eneo4j:build_statement(Query, Params),
%%% '''
-spec build_statement(binary(), #{binary() => any()}) -> statement().
build_statement(Query, Params) ->
    eneo4j_worker:build_statement(Query, Params).

% Private functions

call_wpool(Request) ->
    {ok, HttpStatusCode, Response} = wpool:call(eneo4j_workers_pool, Request, available_worker),
    process_response(Request, HttpStatusCode, Response).

process_response(
    {rollback_transaction, _},
    HttpStatusCode,
    Response = #{<<"results">> := [], <<"errors">> := []}
) when HttpStatusCode =/= 404 ->
    {ok, Response};
process_response(_, 404, Response) ->
    {error, {transaction_not_found, Response}};
process_response(_, _, Response) ->
    case eneo4j_response:is_successful(Response) of
        true -> {ok, Response};
        Error -> Error
    end.
