eneo4j
=====

# CI Status

![Erlang CI](https://github.com/spawnfest/eneo4j/workflows/Erlang%20CI/badge.svg)
[![codecov](https://codecov.io/gh/spawnfest/eneo4j/branch/master/graph/badge.svg)](https://codecov.io/gh/spawnfest/eneo4j)


## Description

This project is prepared during a Spawnfest competition.
It aims to implement easy communication with neo4j database using its [HTTP API](https://neo4j.com/docs/http-api/current/introduction/). This library is written in Erlang so that it can be used from both Erlang and Elixir.

### Documentation

Edoc documentation is provided for this project [here](https://spawnfest.github.io/eneo4j/).

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

## Benchmark

There is a simple benchmark provided to the project. To execute it go to `test/eneo4j_benchmark_SUITE.erl` and uncomment `init_per_suite/1` config. Then manually extract data results (eg using regex) and paste it to `data.csv`. The scenario is inserting 10 nodes 1000 times and checking execution query times.
Executing this test on my laptop gives following result:


![Benchmark result](/benchmark/BenchmarkResult.png)

## Test

To set up databases for testing purposes run `make setup_db`.
It may take up to a minute for neo4j to set up, if you set it up for the first time, depending on your environment's performance you might need to wait for it to run properly, therefore if you get timeouts in the tests just rerun them after about a minute.
To run the tests run `make precommit`.
For further details see Makefile.

## Use

This section describes how to build requests and what kind of answer to expect.

### [Discovery API](https://neo4j.com/docs/http-api/current/discovery/)

This part of the API could be used to check the connection.
It returns basic information about the requested neo4j instance.

```erlang
eneo4j:discovery_api().

% Result is:
#{
  <<"bolt_direct">> => <<"bolt://localhost:7687">>,
  <<"bolt_routing">> => <<"neo4j://localhost:7687">>,
  <<"neo4j_edition">> => <<"community">>,
  <<"neo4j_version">> => <<"4.1.1">>,
  <<"transaction">> =><<"http://localhost:7470/db{databaseName}/tx">>
}
```

## [Transactions workflow](https://neo4j.com/docs/http-api/current/actions/transaction-flow/)

To understand the requests see this Cypher transaction flow:

![Transactions flow](https://neo4j.com/docs/http-api/current/images/http-cypher-transaction-api-flow.png)

The following sections show how to make those actions happen from Erlang API level.

### [Begin & commit](https://neo4j.com/docs/http-api/current/actions/begin-and-commit-a-transaction-in-one-request/) transaction

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


### [Begin](https://neo4j.com/docs/http-api/current/actions/begin-a-transaction/) transaction

If you want to just open a transaction and later on decide whether to add more statements to it, commit it or rollback it.

To begin a transaction:

```erlang
...
  QueryGetPersonsNames = <<"MATCH (n:Person) RETURN n.name">>,
  Statement = eneo4j:build_statement(QueryGetPersonsNames, #{}),
  {ok, Response} = eneo4j:begin_transaction([Statement]),
...
```

### [Run](https://neo4j.com/docs/http-api/current/actions/execute-statements-in-an-open-transaction/) or [keep alive](https://neo4j.com/docs/http-api/current/actions/reset-transaction-timeout-of-an-open-transaction/) transactions

When you have a query started  you may want to run more queries inside it:

```erlang

% You have opened a transaction:
{ok, BeginResponse} = eneo4j:begin_transaction([Statement]),

% You extract `run_queries_link` from the Begin query result
{ok, RunLink} = eneo4j_response:get_run_queries_link(BeginResponse),
% Let's assume you have a list of statements under Statements and OtherStatements variables
{ok, RunResponse} = eneo4j:run_queries_inside_transaction(Statements, RunLink),

% Later on you can run more queries inside the same transaction.
% You can use run_queries_inside_transaction result to get a run link

{ok, RunLink2} = eneo4j_response:get_run_queries_link(RunResponse),
{ok, AnotherRunResponse} = eneo4j:run_queries_inside_transaction(OtherStatements, RunLink2),
```

Since by default queries are expiring after 60 seconds you may want to keep it alive like this:

```erlang
% You have opened a transaction:
{ok, BeginResponse} = eneo4j:begin_transaction([Statement]),

% You extract `run_queries_link` from the Begin query result
{ok, RunLink} = eneo4j_response:get_run_queries_link(BeginResponse),

%Lets wait for a while:
timer:sleep(40 * 1000),

% You do not want the transaction to timeout, but you do not want to send more queries yet you may just keep it alive
{ok, KeepAliveResponse} = eneo4j:keep_alive_transaction(RunLink),

%Lets wait for a while:
timer:sleep(40 * 1000),

% You may later on commit, run queries or rollback transaction after that.

{ok, RunLink2} = eneo4j_response:get_run_queries_link(KeepAliveResponse),
{ok, AnotherRunResponse} = eneo4j:run_queries_inside_transaction(OtherStatements, RunLink2),
...
```

### [Commit](https://neo4j.com/docs/http-api/current/actions/commit-an-open-transaction/) transaction

When you have a transaction opened successfully result or run query result, you need to extract the commit query link from the response to commit this query:

```erlang
...
  {ok, Response} = eneo4j:begin_transaction([Statement]),
  {ok, CommitLink} = eneo4j_response:get_commit_transaction_link(Response),
  % You may add statements when committing a transaction
  Statements = [],
  eneo4j:commit_transaction(Statements, CommitLink),
...
```

### [Rollback](https://neo4j.com/docs/http-api/current/actions/rollback-an-open-transaction/) an open transaction


```erlang
% Let's assume you have a list of statements under Statements variable
{ok, BeginResponse} = eneo4j:begin_transaction(Statements),

% But you decided that you do not want to commit those changes but rather rollback therm
% Then using BeginResponse you get a RollbackLink
{ok, RollbackLink} = eneo4j_response:get_rollback_transaction_link(BeginResponse),
% And you use that link to rollback the transaction
{ok, _} = eneo4j:rollback_transaction(RollbackLink).
```
