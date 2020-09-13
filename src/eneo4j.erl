-module(eneo4j).

-export([
    discvery_api/0,
    begin_and_commit_transaction/1,
    begin_transaction/1,
    run_queries_inside_transaction/2,
    keep_alive_transaction/1,
    commit_transaction/2
]).

-export([
    build_statement/2,
    build_statement/3
]).

-ignore_xref([
    {?MODULE, discvery_api, 0},
    {?MODULE, begin_and_commit_transaction, 1},
    {?MODULE, begin_transaction, 1},
    {?MODULE, run_queries_inside_transaction, 2},
    {?MODULE, keep_alive_transaction, 1},
    {?MODULE, commit_transaction, 2},
    {?MODULE, build_statement, 2},
    {?MODULE, build_statement, 3}
]).

discvery_api() ->
    {ok, 200, Response} = wpool:call(eneo4j_workers_pool, discovery_api, available_worker),
    Response.

begin_and_commit_transaction(Statements) ->
    Request = {begin_and_commit_transaction, Statements},
    call_wpool(Request).

begin_transaction(Statements) ->
    Request = {begin_transaction, Statements},
    call_wpool(Request).

run_queries_inside_transaction(Statements, RunLink) ->
    Request = {{run_queries, RunLink}, Statements},
    call_wpool(Request).

keep_alive_transaction(RunLink) ->
    run_queries_inside_transaction([], RunLink).

commit_transaction(Statements, CommitLink) ->
    Request = {{commit_transaction, CommitLink}, Statements},
    call_wpool(Request).

build_statement(Query, Params, IncludeStats) ->
    eneo4j_worker:build_statement(Query, Params, IncludeStats).

build_statement(Query, Params) ->
    eneo4j_worker:build_statement(Query, Params).

% Private functions

call_wpool(Request) ->
    HttpStatusCode = get_http_status(Request),
    {ok, HttpStatusCode, Response} = wpool:call(eneo4j_workers_pool, Request, available_worker),
    process_response(Response).

get_http_status({begin_transaction, _}) -> 201;
get_http_status(_Request) -> 200.

process_response(Response) ->
    case eneo4j_reponse:is_successful(Response) of
        true -> {ok, Response};
        Error -> Error
    end.
