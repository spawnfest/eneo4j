-module(eneo4j_reponse).

-export([is_successful/1]).

is_successful(#{<<"errors">> := []}) -> true;
is_successful(#{<<"errors">> := Errors}) -> {error, Errors}.

% eof
