%%%-------------------------------------------------------------------
%% @doc eneo4j public API
%% @end
%%%-------------------------------------------------------------------

-module(eneo4j_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Config = #{
        eneo4j_worker_config => get_and_validate_workers_config(),
        eneo4j_workers_count => get_and_validate_workers_numer()
    },
    start(Config).

start(#{eneo4j_worker_config := Error = {error, _}}) -> Error;
start(#{eneo4j_workers_count := Error = {error, _}}) -> Error;
start(Config) -> eneo4j_sup:start_link(Config).

stop(_State) ->
    ok.

%% internal functions

get_and_validate_workers_config() ->
    Eneo4jWorkerConfig = persistent_term:get(eneo4j_worker_config, undefined),
    validate_workers_config(Eneo4jWorkerConfig).

validate_workers_config(WorkerConfig = #{url := Url, db := DB}) when is_list(Url), is_list(DB) ->
    WorkerConfig;
validate_workers_config(
    WorkerConfig = #{url := Url, db := DB, user := User, password := Password}
) when is_list(Url), is_list(DB), is_list(User), is_list(Password) ->
    WorkerConfig;
validate_workers_config(undefined) ->
    {error, {eneo4j_worker_config, undefined}};
validate_workers_config(WorkerConfig) ->
    {error, {"eneo4j_worker_config is miss configured", WorkerConfig}}.

get_and_validate_workers_numer() ->
    WorkersNum = persistent_term:get(eneo4j_workers_count, undefined),
    validate_workers_numer(WorkersNum).

validate_workers_numer(WorkersNum) when is_integer(WorkersNum), WorkersNum > 0 ->
    WorkersNum;
validate_workers_numer(undefined) ->
    {error, {eneo4j_worker_config, undefined}};
validate_workers_numer(WorkersNum) ->
    {error, {"eneo4j_workers_count is miss configured", WorkersNum}}.
