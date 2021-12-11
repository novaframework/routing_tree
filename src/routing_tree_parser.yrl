Nonterminals url_path path_segment query basic_element.

Terminals 'divider' 'binding' 'wildcard' 'segment' 'query_start' 'ampersand' 'equals'.

Rootsymbol url_path.


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

%% Remove trailing slash
url_path ->
    path_segment 'divider': '$1'.
url_path ->
    path_segment: '$1'.


Erlang code.
