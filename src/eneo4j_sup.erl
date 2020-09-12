%%%-------------------------------------------------------------------
%% @doc eneo4j top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eneo4j_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

init(#{eneo4j_worker_config := Eneo4jWorkerConfig, eneo4j_workers_count := Eneo4jWorkersCount}) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => eneo4j_workers_pool,
            start =>
                {wpool, start_pool, [
                    eneo4j_workers_pool,
                    [
                        {worker, {eneo4j_worker, Eneo4jWorkerConfig}},
                        {workers, Eneo4jWorkersCount}
                    ]
                ]}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
