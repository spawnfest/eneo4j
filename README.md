eneo4j
=====

# CI Status

![Erlang CI](https://github.com/spawnfest/eneo4j/workflows/Erlang%20CI/badge.svg)
[![codecov](https://codecov.io/gh/spawnfest/eneo4j/branch/master/graph/badge.svg)](https://codecov.io/gh/spawnfest/eneo4j)
---

# Description

This project is prepared during a Spawnfest competition.
It aims to implement easy communication with neo4j database using its [HTTP API](https://neo4j.com/docs/http-api/current/introduction/).

# Installation

To `rebar.config` add

```erlang
{deps, [
    ....
    {eneo4j, ".*", {git, "git://github.com/spawnfest/eneo4j.git", {branch, "master"}}}
]}.
```

# Configuration

