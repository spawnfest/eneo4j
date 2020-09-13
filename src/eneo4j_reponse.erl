-module(eneo4j_reponse).

-export([
    is_successful/1,
    get_commit_transaction_link/1,
    get_run_queries_link/1
]).

-ignore_xref([
    {?MODULE, is_successful, 1},
    {?MODULE, get_commit_transaction_link, 1},
    {?MODULE, get_run_queries_link, 1}
]).

is_successful(#{<<"errors">> := []}) -> true;
is_successful(#{<<"errors">> := Errors}) -> {error, Errors}.

get_commit_transaction_link(#{<<"errors">> := [], <<"commit">> := CommitLink}) ->
    {ok, binary:bin_to_list(CommitLink)};
get_commit_transaction_link(#{<<"errors">> := []} = Result) ->
    {error, {no_commit_link, Result}};
get_commit_transaction_link(Error) ->
    Error.

get_run_queries_link(Response) ->
    CommitLink = get_commit_transaction_link(Response),
    change_commit_link_to_run_queries_link(CommitLink).

change_commit_link_to_run_queries_link({ok, CommitLink}) ->
    [Link | _] = string:replace(CommitLink, "/commit", ""),
    {ok, Link};
change_commit_link_to_run_queries_link(Error) ->
    Error.
