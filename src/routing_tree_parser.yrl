Nonterminals url_path path_segment optional_segment basic_element.

Terminals 'divider' 'binding' 'wildcard' 'optional_left' 'optional_right' 'segment'.

Rootsymbol url_path.


basic_element ->
    'binding': '$1'.
basic_element ->
    'segment': '$1'.

%% Optional segments can only be at the end of this
optional_segment ->
    'divider' 'optional_left' basic_element 'optional_right': [optional('$3')].
optional_segment ->
    'divider' 'optional_left' basic_element 'optional_right' optional_segment: [optional('$3')|'$5'].

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
