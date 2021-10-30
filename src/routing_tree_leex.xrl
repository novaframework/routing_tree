Definitions.
PathSegment = (_|[a-zA-Z]|[0-9])*

Rules.

\/              : {token, {'divider', TokenLine}}.
\:{PathSegment} : {token, {'binding', TokenLine, strip(TokenChars)}}.
\[\.\.\.\]      : {token, {'wildcard', TokenLine, '...'}}.
\[              : {token, {'optional_left', TokenLine}}.
\]              : {token, {'optional_right', TokenLine}}.
\?              : {token, {'query_start', TokenLine}}.
\&              : {token, {'ampersand', TokenLine}}.
\=              : {token, {'equals', TokenLine}}.
\#              : {token, {'fragment', TokenLine}}.
{PathSegment}   : {token, {'segment', TokenLine, TokenChars}}.
.               : {error, {unknown_symbol, TokenChars}}.

Erlang code.

strip([$:|Tl]) ->
    Tl;
strip(A) ->
    A.
