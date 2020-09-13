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

-spec discovery_api() -> discovery_api_response().
discovery_api() ->
    {ok, 200, Response} = wpool:call(eneo4j_workers_pool, discovery_api, available_worker),
    Response.

-spec begin_and_commit_transaction(statements()) -> query_result().
begin_and_commit_transaction(Statements) ->
    Request = {begin_and_commit_transaction, Statements},
    call_wpool(Request).

-spec begin_transaction(statements()) -> query_result_with_commit().
begin_transaction(Statements) ->
    Request = {begin_transaction, Statements},
    call_wpool(Request).

-spec run_queries_inside_transaction(statements(), eneo4j_response:run_queries_link()) ->
    query_result_with_commit().
run_queries_inside_transaction(Statements, RunLink) ->
    Request = {{run_queries, RunLink}, Statements},
    call_wpool(Request).

-spec keep_alive_transaction(eneo4j_response:run_queries_link()) -> query_result_with_commit().
keep_alive_transaction(RunLink) ->
    run_queries_inside_transaction([], RunLink).

-spec commit_transaction(statements(), eneo4j_response:commit_transaction_link()) ->
    query_result_with_commit().
commit_transaction(Statements, CommitLink) ->
    Request = {{commit_transaction, CommitLink}, Statements},
    call_wpool(Request).

-spec rollback_transaction(eneo4j_response:rollback_transaction_link()) ->
    query_result_with_commit().
rollback_transaction(RollbackLink) ->
    Request = {rollback_transaction, RollbackLink},
    call_wpool(Request).

-spec build_statement(binary(), #{binary() => any()}, boolean()) -> statement().
build_statement(Query, Params, IncludeStats) ->
    eneo4j_worker:build_statement(Query, Params, IncludeStats).

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
