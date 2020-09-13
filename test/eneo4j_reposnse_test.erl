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

% eof
