-module(eneo4j_reponse).

-export([
    is_successful/1,
    get_commit_transaction_link/1
]).

is_successful(#{<<"errors">> := []}) -> true;
is_successful(#{<<"errors">> := Errors}) -> {error, Errors}.

get_commit_transaction_link(#{<<"errors">> := [], <<"commit">> := CommitLink}) ->
    {ok, binary:bin_to_list(CommitLink)};
get_commit_transaction_link(#{<<"errors">> := []} = Result) ->
    {error, {no_commit_link, Result}};
get_commit_transaction_link(Error) ->
    Error.
