-module(eneo4j_worker).

-behaviour(gen_server).

-export([
    start_link/1
]).

-ignore_xref([
    {?MODULE, start_link, 1}
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export([
    build_statement/2,
    build_statement/3
]).

-compile({inline, [build_content_headers/0]}).

-export_type([eneo4j_worker_config/0]).

-type start_error() :: {already_started, pid()} | term().
-type start_result() :: {ok, pid()} | ignore | {error, start_error()}.
-type url() :: string().
-type header() :: {Key :: bitstring(), Value :: bitstring()}.
-type headers() :: [header()].

-type eneo4j_worker_config() :: #{
    url := url(),
    db := string(),
    user => string(),
    password => string()
}.

-type eneo4j_worker_state() :: #{
    url := url(),
    db := string(),
    headers := headers(),
    user => string(),
    password => string()
}.

-type cypher_query() :: binary().
-type query_params() :: #{atom() => any()}.
-type statement() :: #{
    statement := cypher_query(),
    parameters := query_params(),
    includeStats => boolean()
}.

-type statements() :: [statement()].
-type request_type() :: begin_and_commit_transaction | begin_transaction.
-type eneo4j_call() :: discovery_api | {request_type(), statements()}.
-type response() :: {ok, pos_integer(), map()} | {error, Reason :: any()}.

-spec start_link(eneo4j_worker_config()) -> start_result().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec init(eneo4j_worker_config()) -> {ok, eneo4j_worker_state()}.
init(State = #{user := User, password := Password}) ->
    Headers = build_content_headers(User, Password),
    {ok, State#{headers => Headers}};
init(State = #{}) ->
    Headers = build_content_headers(),
    {ok, State#{headers => Headers}}.

-spec handle_call(eneo4j_call(), From :: term(), eneo4j_worker_state()) ->
    {reply, response(), eneo4j_worker_state()}.
handle_call(discovery_api, _From, State = #{url := Url, headers := Headers}) ->
    Response = send_request_and_process_response(get, Url, Headers, <<"">>),
    {reply, Response, State};
handle_call({RequestType, Statements}, _From, State = #{headers := Headers}) ->
    Request = build_request(Statements),
    FullUrl = build_url(RequestType, State),
    Response = send_request_and_process_response(post, FullUrl, Headers, Request),
    {reply, Response, State}.

handle_cast(Cast, State) ->
    logger:warning("Unexpected cast ~p", [Cast]),
    {noreply, State}.

handle_info(Info, State) ->
    logger:warning("Unexpected message ~p", [Info]),
    {noreply, State}.

-spec build_statement(cypher_query(), query_params()) -> statement().
build_statement(Query, Params) ->
    build_statement(Query, Params, false).

-spec build_statement(cypher_query(), query_params(), boolean()) -> statement().
build_statement(Query, Params, IncludeStats) ->
    #{
        statement => Query,
        parameters => Params,
        includeStats => IncludeStats
    }.

% Private functions

build_content_headers(User, Password) ->
    [
        build_auth_header(User, Password)
        | build_content_headers()
    ].

build_content_headers() ->
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Accept">>, <<"application/json">>}
    ].

build_auth_header(User, Password) ->
    Basic = <<"Basic ">>,
    Val = base64:encode(User ++ ":" ++ Password),
    {<<"Authorization">>, <<Basic/binary, Val/binary>>}.

build_request(Statements) when is_list(Statements) ->
    RawRequest = #{statements => Statements},
    jiffy:encode(RawRequest).

build_url({Type, Link}, _) when commit_transaction == Type orelse run_queries == Type ->
    Link;
build_url(begin_transaction, #{url := Url, db := DB}) ->
    Url ++ "/db/" ++ DB ++ "/tx";
build_url(begin_and_commit_transaction, #{url := Url, db := DB}) ->
    Url ++ "/db/" ++ DB ++ "/tx/commit".

send_request_and_process_response(Method, Url, Headers, ReqBody) ->
    case hackney:request(Method, Url, Headers, ReqBody, []) of
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            DecodedBody = jiffy:decode(ResponseBody, [return_maps]),
            {ok, StatusCode, DecodedBody};
        Response = {error, _Reason} ->
            Response
    end.
