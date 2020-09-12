%%%-------------------------------------------------------------------
%% @doc eneo4j public API
%% @end
%%%-------------------------------------------------------------------

-module(eneo4j_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    eneo4j_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
