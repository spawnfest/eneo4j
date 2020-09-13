-module(eneo4j_response).

-export([
    is_successful/1,
    get_commit_transaction_link/1,
    get_rollback_transaction_link/1,
    get_run_queries_link/1
]).

-ignore_xref([
    {?MODULE, is_successful, 1},
    {?MODULE, get_commit_transaction_link, 1},
    {?MODULE, get_rollback_transaction_link, 1},
    {?MODULE, get_run_queries_link, 1}
]).

-export_type([commit_transaction_link/0, rollback_transaction_link/0, run_queries_link/0]).

-type commit_transaction_link() :: string().
-type rollback_transaction_link() :: string().
-type run_queries_link() :: string().
-type response() :: eneo4j:response().
-type response_with_commit() :: eneo4j:response_with_commit().
-type get_result_link(ResultLink) :: {ok, ResultLink} | {error, Reason :: any()}.

-spec is_successful(response() | response_with_commit()) -> true | {error, Reason :: any()}.
is_successful(#{<<"errors">> := []}) -> true;
is_successful(#{<<"errors">> := Errors}) -> {error, Errors}.

-spec get_commit_transaction_link(response_with_commit() | {error, any()}) ->
    get_result_link(commit_transaction_link()).
get_commit_transaction_link(#{<<"errors">> := [], <<"commit">> := CommitLink}) ->
    {ok, binary:bin_to_list(CommitLink)};
get_commit_transaction_link(#{<<"errors">> := []} = Result) ->
    {error, {no_commit_link, Result}};
get_commit_transaction_link({error, _} = Error) ->
    Error.

-spec get_rollback_transaction_link(response_with_commit()) ->
    get_result_link(rollback_transaction_link()).
get_rollback_transaction_link(Response) ->
    get_run_queries_link(Response).

-spec get_run_queries_link(response_with_commit()) -> get_result_link(run_queries_link()).
get_run_queries_link(Response) ->
    CommitLink = get_commit_transaction_link(Response),
    change_commit_link_to_run_queries_link(CommitLink).

change_commit_link_to_run_queries_link({ok, CommitLink}) ->
    [Link | _] = string:replace(CommitLink, "/commit", ""),
    {ok, Link};
change_commit_link_to_run_queries_link(Error) ->
    Error.
