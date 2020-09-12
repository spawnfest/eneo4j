-module(eneo4j).

-export([discvery_api/0]).

discvery_api() ->
    wpool:call(eneo4j_workers_pool, discovery_api, available_worker).
