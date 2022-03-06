Nonterminals path_segment query basic_element.

Terminals 'divider' 'binding' 'wildcard' 'segment' 'query_start' 'ampersand' 'equals'.

Rootsymbol path_segment.


basic_element ->
    'binding': '$1'.
basic_element ->
    'segment': '$1'.

query ->
    'segment' 'equals' 'segment': [{'query', '$1', '$3'}].
query ->
    'segment' 'equals' 'segment' 'ampersand' query: [{'query', '$1', '$3'}|'$5'].

path_segment ->
    'query_start' query : '$2'.
path_segment ->
    'divider': [{'segment', 1, ""}].
path_segment ->
    'divider' 'wildcard': ['$2'].
path_segment ->
    'divider' basic_element: ['$2'].
path_segment ->
    'divider' basic_element path_segment: ['$2'|'$3'].

Erlang code.
