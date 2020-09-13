-module(eneo4j_reposnse_test).

-include_lib("eunit/include/eunit.hrl").

-define(DUMMY_SUCCESS_RESPONSE, #{<<"errors">> => [], <<"results">> => []}).
-define(ACTUALL_SUCCES_RESPONSE, #{
    <<"errors">> => [],
    <<"results">> => [
        #{
            <<"columns">> => [<<"n">>],
            <<"data">> => [
                #{
                    <<"meta">> => [
                        #{<<"deleted">> => false, <<"id">> => 3, <<"type">> => <<"node">>}
                    ],
                    <<"row">> => [#{<<"name">> => <<"Andy">>, <<"title">> => <<"Developer">>}]
                }
            ]
        }
    ]
}).

-define(ACTUAL_FAILURE_REPOSNSE, #{
    <<"errors">> => [
        #{
            <<"code">> =>
                <<"Neo.ClientError.Statement.SyntaxError">>,
            <<"message">> =>
                <<"Invalid input 'X' (line 1, column 6 (offset: 5))\n\"MATCHXD    (n:Person) RETURN n.name\"\n      ^">>
        }
    ],
    <<"results">> => []
}).

is_successful_returns_true_when_no_errors_were_found_test() ->
    ?assertEqual(true, eneo4j_reponse:is_successful(?DUMMY_SUCCESS_RESPONSE)).

is_successful_returns_true_when_no_errors_were_found_complex_response_test() ->
    ?assertEqual(true, eneo4j_reponse:is_successful(?ACTUALL_SUCCES_RESPONSE)).

is_successful_returns_error_when_errors_were_found_test() ->
    ?assertMatch({error, _}, eneo4j_reponse:is_successful(?ACTUAL_FAILURE_REPOSNSE)).

-define(BEGIN_TRANSACTION_SUCCESS_LINK, "http://localhost:7470/db/neo4j/tx/397").
-define(BEGIN_TRANSACTION_SUCCESS, #{
    <<"commit">> =>
        <<"http://localhost:7470/db/neo4j/tx/397/commit">>,
    <<"errors">> => [],
    <<"results">> => [
        #{
            <<"columns">> => [<<"n.name">>],
            <<"data">> => [
                #{<<"meta">> => [null], <<"row">> => [<<"Andy">>]},
                #{
                    <<"meta">> => [null],
                    <<"row">> => [<<"Andy">>]
                }
            ]
        }
    ],
    <<"transaction">> => #{
        <<"expires">> =>
            <<"Sun, 13 Sep 2020 11:17:12 GMT">>
    }
}).

-define(BEGIN_TRANSACTION_FAIL,
    {error, [
        #{
            <<"code">> => <<"Neo.ClientError.Statement.SyntaxError">>,
            <<"message">> =>
                <<"Invalid input 'X' (line 1, column 6 (offset: 5))\n\"MATCHXD (n:Person) RETURN n.name\"\n      ^">>
        }
    ]}
).

get_commit_transaction_link_returns_link_from_successfully_begin_query_test() ->
    {ok, ContentLink} = eneo4j_reponse:get_commit_transaction_link(?BEGIN_TRANSACTION_SUCCESS),
    ?assertMatch(#{host := "localhost"}, uri_string:parse(ContentLink)).

get_commit_transaction_link_returns_link_from_successfully_begin_and_commit_query_test() ->
    Result = eneo4j_reponse:get_commit_transaction_link(?ACTUALL_SUCCES_RESPONSE),
    ?assertMatch({error, {no_commit_link, _}}, Result).

get_commit_transaction_link_returns_error_from_error_begin_query_test() ->
    ?assertMatch({error, _}, eneo4j_reponse:get_commit_transaction_link(?BEGIN_TRANSACTION_FAIL)).

get_run_queries_link_return_link_on_begin_transaction_success_test() ->
    {ok, RunLink} = eneo4j_reponse:get_run_queries_link(?BEGIN_TRANSACTION_SUCCESS),
    ?assertEqual(?BEGIN_TRANSACTION_SUCCESS_LINK, RunLink).

get_run_queries_link_return_link_on_begin_transaction_error_test() ->
    Result = eneo4j_reponse:get_run_queries_link(?BEGIN_TRANSACTION_FAIL),
    ?assertMatch({error, _}, Result).

get_run_queries_link_return_link_from_run_queries_test() ->
    BinLink = <<"http://localhost:7470/db/neo4j/tx/397">>,
    LstLink = binary:bin_to_list(BinLink),
    RunQueriesResult = #{
        <<"commit">> => BinLink,
        <<"errors">> => []
    },
    ?assertEqual({ok, LstLink}, eneo4j_reponse:get_run_queries_link(RunQueriesResult)).

get_run_queries_link_returns_error_on_error_input_test() ->
    Error = {error, some_reason},
    ?assertEqual(Error, eneo4j_reponse:get_run_queries_link(Error)).

get_rollback_transaction_link_return_link_on_begin_transaction_success_test() ->
    {ok, RunLink} = eneo4j_reponse:get_rollback_transaction_link(?BEGIN_TRANSACTION_SUCCESS),
    ?assertEqual(?BEGIN_TRANSACTION_SUCCESS_LINK, RunLink).

get_rollback_transaction_link_return_link_on_begin_transaction_error_test() ->
    Result = eneo4j_reponse:get_rollback_transaction_link(?BEGIN_TRANSACTION_FAIL),
    ?assertMatch({error, _}, Result).

get_rollback_transaction_link_return_link_from_run_queries_test() ->
    BinLink = <<"http://localhost:7470/db/neo4j/tx/397">>,
    LstLink = binary:bin_to_list(BinLink),
    RunQueriesResult = #{
        <<"commit">> => BinLink,
        <<"errors">> => []
    },
    ?assertEqual({ok, LstLink}, eneo4j_reponse:get_rollback_transaction_link(RunQueriesResult)).

get_rollback_transaction_link_returns_error_on_error_input_test() ->
    Error = {error, some_reason},
    ?assertEqual(Error, eneo4j_reponse:get_rollback_transaction_link(Error)).

% eof
