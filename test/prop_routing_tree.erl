-module(prop_routing_tree).

-include_lib("proper/include/proper.hrl").

-include("../include/routing_tree.hrl").

host_tree(Host, Path, Comparator, Value) ->
    TokenizedPath = string:tokens(Path, "/"),
    #host_tree{hosts =
                   [{Host,
                     #routing_tree{tree = [host_tree_aux(TokenizedPath, Comparator, Value)]}}],
               options = #{convert_to_binary => false, use_strict => false}}.

host_tree_aux([[58 | Segment] | Rest], Comparator, Value) when length(Rest) > 0 ->
    #node{segment = Segment,
          value = [],
          children = [host_tree_aux(Rest, Comparator, Value)],
          is_binding = true,
          is_wildcard = false};
host_tree_aux([[58 | Segment] | Rest], Comparator, Value) when length(Rest) == 0 ->
    #node{segment = Segment,
          value = [#node_comp{comparator = Comparator, value = Value}],
          children = [],
          is_binding = true,
          is_wildcard = false};
host_tree_aux([Segment | Rest], Comparator, Value) when length(Rest) == 0 ->
    #node{segment = Segment,
          value = [#node_comp{comparator = Comparator, value = Value}],
          children = [],
          is_binding = false,
          is_wildcard = false};
host_tree_aux([Segment | Rest], Comparator, Value) when length(Rest) > 0 ->
    #node{segment = Segment,
          value = [],
          children = [host_tree_aux(Rest, Comparator, Value)],
          is_binding = false,
          is_wildcard = false}.

model_binding(String) ->
    [47, 58] ++ bindings(String).

model_non_binding(String) ->
    [47] ++ path(String).

path([Char])
    when Char >= $a andalso Char =< $z
         orelse Char >= $A andalso Char =< $Z
         orelse Char >= $0 andalso Char =< $9 ->
    [Char];
path(_) ->
    [$a].

bindings([Char]) when Char >= $a andalso Char =< $z ->
    [Char];
bindings(_) ->
    [$a].

path_generator(_, 0, Acc) ->
    lists:flatten(Acc);
path_generator(Path, N, Acc) when N rem 2 == 0 ->
    path_generator(Path, N - 1, Acc ++ [model_binding(Path)]);
path_generator(Path, N, Acc) when N rem 2 == 1 ->
    path_generator(Path, N - 1, Acc ++ [model_non_binding(Path)]).

method_generator() ->
    oneof([<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>]).

function_generator() ->
    {atom(), atom()}.

prop_insert_non_bindings() ->
    ?FORALL({Host, Path, Comparator, Value, HT},
            {any(), string(), method_generator(), function_generator(), #host_tree{}},
            begin
                Path2 = model_non_binding(Path),
                HostTree = host_tree(Host, Path2, Comparator, Value),
                HostTree == routing_tree:insert(Host, Path2, Comparator, Value, HT)
            end).

prop_insert_bindings() ->
    ?FORALL({Host, Path, Comparator, Value, HT},
            {any(), string(), any(), any(), #host_tree{}},
            begin
                Path2 = model_binding(Path),
                HostTree = host_tree(Host, Path2, Comparator, Value),
                HostTree == routing_tree:insert(Host, Path2, Comparator, Value, HT)
            end).

prop_combine() ->
    ?FORALL({Host, {Path, Number}, Comparator, Value, HT},
            {any(),
             {string(), pos_integer()},
             method_generator(),
             function_generator(),
             #host_tree{}},
            begin
                Path2 = path_generator(Path, Number, []),
                HostTree = host_tree(Host, Path2, Comparator, Value),
                InsertedTree = routing_tree:insert(Host, Path2, Comparator, Value, HT),
                HostTree == InsertedTree
            end).
