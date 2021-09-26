Nonterminals url_path path_segment query optional_segment basic_element.

Terminals 'divider' 'binding' 'wildcard' 'optional_left' 'optional_right' 'segment' 'query_start' 'ampersand' 'equals'.

Rootsymbol url_path.


basic_element ->
    'binding': '$1'.
basic_element ->
    'segment': '$1'.

%% Optional segments can only be at the end of this
optional_segment ->
    'divider' 'optional_left' basic_element 'optional_right': [optional('$3')].
optional_segment ->
    'divider' 'optional_left' basic_element 'optional_right' query: [optional('$3')|'$5'].
optional_segment ->
    'divider' 'optional_left' basic_element 'optional_right' optional_segment: [optional('$3')|'$5'].

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
    optional_segment: '$1'.
path_segment ->
    'divider' basic_element: ['$2'].
path_segment ->
    'divider' basic_element path_segment: ['$2'|'$3'].

url_path ->
    path_segment: '$1'.


Erlang code.

optional(X) ->
    {optional, X}.
