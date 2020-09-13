eneo4j
=====

# CI Status

![Erlang CI](https://github.com/spawnfest/eneo4j/workflows/Erlang%20CI/badge.svg)
[![codecov](https://codecov.io/gh/spawnfest/eneo4j/branch/master/graph/badge.svg)](https://codecov.io/gh/spawnfest/eneo4j)
---

## Description

This project is prepared during a Spawnfest competition.
It aims to implement easy communication with neo4j database using its [HTTP API](https://neo4j.com/docs/http-api/current/introduction/). This library is written in Erlang so that it can be used from both Erlang and Elixir.

## Installation

To `rebar.config` add:

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

### [Discovery API](https://neo4j.com/docs/http-api/current/discovery/)

This part of the API could be used to check the connection.
It returns basic information about the requested neo4j instance.

```erlang
eneo4j:discvery_api().

% Result is:
#{
  <<"bolt_direct">> => <<"bolt://localhost:7687">>,
  <<"bolt_routing">> => <<"neo4j://localhost:7687">>,
  <<"neo4j_edition">> => <<"community">>,
  <<"neo4j_version">> => <<"4.1.1">>,
  <<"transaction">> =><<"http://localhost:7470/db{databaseName}/tx">>
}
```

### [Begin and commit a transaction](https://neo4j.com/docs/http-api/current/actions/begin-and-commit-a-transaction-in-one-request/)

To query neo4j:

```erlang
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
eneo4j:begin_and_commit_transaction([Statement, Statement2]).
```

#### Successful querying example


Let's say we have added a node to the database:

```cypher
CREATE (n:Person { name: 'Andy', title: 'Developer' });
```

And we just want to get all the information about Andy's node we can:

```erlang
% Executing following code:
Query = <<"MATCH (n) WHERE n.name = $name RETURN n">>,
Params = #{<<"name">> => <<"Andy">>},
Statement = eneo4j:build_statement(Query, Params),
eneo4j:begin_and_commit_transaction([Statement]).

% Returns:
{
  ok,
  #{
    <<"errors">> => [],
    <<"results">> => [
      #{
        <<"columns">> => [<<"n">>],
        <<"data">> => [
          #{<<"meta">> => [
            #{<<"deleted">> => false,
            <<"id">> => 3,
            <<"type">> => <<"node">>}],
            <<"row">> => [
              #{<<"name">> => <<"Andy">>,
                <<"title">> => <<"Developer">>
              }
            ]
          }
        ]
      }
    ]
  }
}
```

#### Error querying example

Let's consider an error in a query:

```erlang
% Let's consider creating a query with an error:
  Query = <<"CREATEXD (n:Person);">>,
  Statement = eneo4j:build_statement(Query, #{}, true),
  eneo4j:begin_and_commit_transaction[Statement]).

% It returns full error to let you fix it:
{error,[
  #{
    <<"code">> => <<"Neo.ClientError.Statement.SyntaxError">>,
    <<"message">> => <<"Invalid input 'X' (line 1, column 7 (offset: 6))\n\"CREATEXD (n:Person);\"\n       ^">>
  }
]}
```
<!-- EOF -->
