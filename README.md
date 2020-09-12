eneo4j
=====

# CI Status

![Erlang CI](https://github.com/spawnfest/eneo4j/workflows/Erlang%20CI/badge.svg)
[![codecov](https://codecov.io/gh/spawnfest/eneo4j/branch/master/graph/badge.svg)](https://codecov.io/gh/spawnfest/eneo4j)
---

## Description

This project is prepared during a Spawnfest competition.
It aims to implement easy communication with neo4j database using its [HTTP API](https://neo4j.com/docs/http-api/current/introduction/).

## Installation

To `rebar.config` add

```erlang
{deps, [
    ....
    {eneo4j, ".*", {git, "git://github.com/spawnfest/eneo4j.git", {branch, "master"}}}
]}.
```

### Configuration

There are 2 parameters to be configured:

 - database connection object
 - number of workers in a connection pool

 To configure the eneo4j connection just put those 2 parameters in persistent term storage.

In `your_project_app.erl` in `start/2` just put:

```erlang
start(_StartType, _StartArgs) ->
    ...
    Eneo4jWorkerConfig = #{
        url => "http://localhost:7474",
        db => "neo4j",
        user => "neo4j",
        password => "test"
    },
    persistent_term:put(eneo4j_worker_config, Eneo4jWorkerConfig),
    persistent_term:put(eneo4j_workers_count, 5),
    {ok, _} = application:ensure_all_started(eneo4j),
    ...
```

You may alternatively put this code under a separate supervisor's init.

Do not include `user` and `password` if your neo4j does not have a password configured.

## Use

This section describes how to build requests and what kind of answer to expect.

### Discovery API

This part of the API could be used to check the connection.
It returns basic information about the requested neo4j instance.

```erlang
eneo4j:discvery_api().

{ok, 200,
  #{
    <<"bolt_direct">> => <<"bolt://localhost:7687">>,
    <<"bolt_routing">> => <<"neo4j://localhost:7687">>,
    <<"neo4j_edition">> => <<"community">>,
    <<"neo4j_version">> => <<"4.1.1">>,
    <<"transaction">> =><<"http://localhost:7470/db/{databaseName}/tx">>
  }
}
```

<!-- EOF -->
