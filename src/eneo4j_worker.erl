-module(eneo4j_worker).

-behaviour(gen_server).

-compile({inline, [build_content_headers/0]}).

-type start_error() :: {already_started, pid()} | term().
-type start_result() :: {ok, pid()} | ignore | {error, start_error()}.
-type url() :: bitstring().
-type header() :: {Key :: bitstring(), Value :: bitstring()}.
-type headers() :: [header()].

-type eneo4j_worker_config() :: #{
    url := url(),
    user => string(),
    password => string()
}.

-type eneo4j_worker_state() :: #{
    url := url(),
    headers := headers(),
    user => string(),
    password => string()
}.

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

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

handle_call(discovery_api, _From, State = #{url := Url, headers := Headers}) ->
    Response = hackney:request(get, Url, Headers, <<"">>, []),
    {reply, Response, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

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
