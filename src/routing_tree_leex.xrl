Definitions.
PathSegment = (_|-|\.|~|:|\[|\]|@|!|\$|\'|\(|\)|\*|\+|,|;|\%|[a-zA-Z]|[0-9])*
Divider = (\/)

Rules.

{Divider}+      : {token, {'divider', TokenLine}}.
\[\.\.\.\]      : {token, {'wildcard', TokenLine, '...'}}.
\:{PathSegment} : {token, {'binding', TokenLine, strip(TokenChars)}}.
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
