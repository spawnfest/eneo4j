-module(eneo4j).

-export([discvery_api/0, begin_and_commit_transaction/1]).

-export([build_statement/2, build_statement/3]).

discvery_api() ->
    wpool:call(eneo4j_workers_pool, discovery_api, available_worker).

build_statement(Query, Params, IncludeStats) ->
    eneo4j_worker:build_statement(Query, Params, IncludeStats).

build_statement(Query, Params) ->
    eneo4j_worker:build_statement(Query, Params).

begin_and_commit_transaction(Statements) ->
    wpool:call(eneo4j_workers_pool, {begin_and_commit_transaction, Statements}, available_worker).
