-module(eneo4j_config_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(WORKER_CONFIG, eneo4j_worker_config).
-define(WORKER_COUNT, eneo4j_workers_count).

init_per_suite(Config) ->
    % Ensure that the app is stopped
    application:stop(eneo4j),
    Config.

end_per_suite(_Config) ->
    application:stop(eneo4j),
    ok.

init_per_testcase(_Case, Config) ->
    persistent_term:erase(?WORKER_COUNT),
    persistent_term:erase(?WORKER_CONFIG),
    Config.

end_per_testcase(_Case, _Config) -> ok.

all() ->
    [
        when_workers_count_and_workers_config_are_undefined_error_is_returned,
        when_workers_count_is_undefined_error_is_returned,
        when_workers_config_is_undefined_error_is_returned,
        when_workers_count_is_miss_configured_error_is_returned,
        when_workers_config_is_miss_configured_error_is_returned
    ].

when_workers_count_and_workers_config_are_undefined_error_is_returned(_Config) ->
    assert_error_on_start().

when_workers_count_is_undefined_error_is_returned(_Config) ->
    persistent_term:put(?WORKER_CONFIG, build_random_worker_config()),
    assert_error_on_start().

when_workers_config_is_undefined_error_is_returned(_Config) ->
    persistent_term:put(?WORKER_COUNT, rand:uniform(10)),
    assert_error_on_start().

when_workers_count_is_miss_configured_error_is_returned(_Config) ->
    persistent_term:put(?WORKER_COUNT, -3),
    persistent_term:put(?WORKER_CONFIG, build_random_worker_config()),
    assert_error_on_start().

when_workers_config_is_miss_configured_error_is_returned(_Config) ->
    persistent_term:put(?WORKER_COUNT, rand:uniform(10)),
    Config = build_random_worker_config(),
    persistent_term:put(?WORKER_CONFIG, Config#{db => 1}),
    assert_error_on_start().

% Private functions

assert_error_on_start() ->
    ?assertMatch(
        {error, _},
        application:ensure_all_started(eneo4j)
    ).

build_random_worker_config() ->
    case rand:uniform() of
        X when X < 0.5 ->
            #{
                url => random_string(),
                db => random_string(),
                user => random_string(),
                password => random_string()
            };
        _ ->
            #{
                url => random_string(),
                db => random_string()
            }
    end.

random_string() ->
    Bits = entropy_string:bits(1.0e6, 1.0e9),
    BinStr = entropy_string:random_string(Bits),
    binary_to_list(BinStr).
