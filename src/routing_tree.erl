-module(routing_tree).
-export([
         new/0,
         new/1,
         insert/5,
         lookup/4
        ]).

-include("../include/routing_tree.hrl").

new() ->
    #host_tree{}.

new(Options) ->
    #host_tree{options = Options}.


lookup(Host, Path, Comparator, #host_tree{hosts = Hosts}) ->
    case lists:keyfind(Host, 1, Hosts) of
        false ->
            {error, not_found};
        {_, #routing_tree{tree = Tree}} ->
            lookup_path(Path, Comparator, Tree, {#{}, undefined})
    end.

lookup_path([], Comparator, _, {Bindings, Node}) ->
    case find_comparator(Comparator, Node#node.value) of
        {ok, #node_comp{value = Value}} ->
            {ok, Bindings, Value};
        Error ->
            Error
    end;
lookup_path([Segment|Tl], Comparator, Tree, {Bindings, _}) ->
    case lists:keyfind(Segment, #node.segment, Tree) of
        false ->
            %% We need to check for potential bindings and/or wildcards
            case lists:keyfind(true, #node.is_binding, Tree) of
                false ->
                    {error, not_found};
                #node{segment = Ident, children = Children} = N ->
                    %% This is a bit special since we need to bind the values to this and continue
                    lookup_path(Tl, Comparator, Children, {Bindings#{Ident => Segment}, N})
            end;
        Node ->
            lookup_path(Tl, Comparator, Node#node.children, {Bindings, Node})
    end.


insert(Host, Path, Comparator, Value, #host_tree{hosts = Hosts, options = Options} = HT) ->
    Tokens = case routing_tree_leex:string(Path) of
                 {ok, Tokens0, _} ->
                     Tokens0;
                 {error, {_Line, _Mod, {user, {Error, Msg}}}, _} ->
                     throw({Error, Msg})
             end,
    SyntaxTree = case routing_tree_parser:parse(Tokens) of
                     {ok, STree} ->
                         STree;
                     {error, {_Line0, _Mod0, Message}} ->
                         throw({bad_routingfile, lists:concat(Message)})
                 end,
    CompNode = #node_comp{comparator = Comparator,
                          value = Value},
    RT =
        case lists:keyfind(Host, 1, Hosts) of
            false -> #routing_tree{};
            {_, RoutingTree} -> RoutingTree
        end,
    RT0 = RT#routing_tree{tree = insert(SyntaxTree, CompNode, RT#routing_tree.tree, Options)},
    HT#host_tree{hosts = [{Host, RT0}|lists:keydelete(Host, 1, Hosts)]}.

insert([], _CompNode, Tree,  _Options) -> Tree;
insert([{Type, _, Ident}|Tl], CompNode, Siblings, Options = #{use_strict := UseStrict}) ->
    case UseStrict of
        true ->
            case check_conflicting_nodes(Type, value(Ident, Options), Tl, CompNode, Siblings) of
                {conflict, Nodes} ->
                    throw({non_deterministic_paths, {Ident, CompNode}, Nodes});
                {duplicate, Nodes} ->
                    throw({duplicated_paths, {Ident, CompNode}, Nodes});
                false ->
                    %% Continue
                    ok
            end;
        _ ->
            ok
    end,

    case lists:keyfind(value(Ident, Options), #node.segment, Siblings) of
        false ->
            %% Nothing found - Just add the tree
            case Tl of
                [] ->
                    [#node{segment = value(Ident, Options), is_binding = Type == binding, is_wildcard = Type == wildcard,
                           value = [CompNode]} | Siblings];
                _ ->
                    [#node{segment = value(Ident, Options), is_binding = Type == binding, is_wildcard = Type == wildcard,
                           children = insert(Tl, CompNode, [], Options)} | Siblings]
            end;
        Node ->
            case Tl of
                [] ->
                    case find_comparator(CompNode#node_comp.comparator, Node#node.value) of
                        {error, not_found} ->
                            [Node#node{value = [CompNode|Node#node.value], is_binding = Type == binding,
                                       is_wildcard = Type == wildcard} | lists:delete(Node, Siblings)];
                        {ok, _NodeComp} ->
                            %% Warn if we're in non-strict mode or exit if we are
                            case maps:get(use_strict, Options, false) of
                                false ->
                                    %% Do nothing - Should we overwrite?
                                    Siblings;
                                _ ->
                                    throw({duplicated_paths})
                            end
                    end;
                _ ->
                    [Node#node{children = insert(Tl, CompNode, Node#node.children, Options),
                               is_binding = Type == binding, is_wildcard = Type == wildcard} | lists:delete(Node, Siblings)]
            end
    end.


%% PRIVATE
find_comparator(_, []) -> {error, not_found};
find_comparator(Comparator, [#node_comp{comparator = Comparator}=Node|_Tl]) ->
    {ok, Node};
find_comparator(Comparator, [_Node|Tl]) ->
    find_comparator(Comparator, Tl).

check_conflicting_nodes(_, _, _, _, []) -> false;
check_conflicting_nodes(segment, Ident, _Tl, CompNode, Siblings) ->
    first(fun(#node{value = Value, segment = Segment, is_binding = false, is_wildcard = false} = Node) ->
                  {[ C || #node_comp{comparator = C} <- Value,
                          C == CompNode#node_comp.comparator ] /= [] andalso Segment == Ident, {duplicate, Node}};
             (#node{is_binding = true} = Node) ->
                  {true, {conflict, Node}};
             (#node{is_wildcard = true} = Node) ->
                  {true, {conflict, Node}}
          end, Siblings);
check_conflicting_nodes(binding, Ident, _Tl, CompNode, Siblings) ->
    first(fun(#node{is_binding = true, value = Value, segment = Segment}) -> Segment == Ident andalso
                                                                                 [ X || #node_comp{comparator = C, value = X} <- Value,
                                                                                        C == CompNode#node_comp.comparator ] /= [];
             (#node{is_wildcard = true}) -> true;
             (#node{value = Value} = Node) ->
                  {[ X || #node_comp{comparator = C, value = X} <- Value, C == CompNode#node_comp.comparator ] /= [], {conflict, Node}}
          end, Siblings);
check_conflicting_nodes(_, _, _, _, _) -> false.



first(_Fun, []) -> false;
first(Fun, [Elem|Tl]) ->
    case Fun(Elem) of
        {true, Res} -> Res;
        _ -> first(Fun, Tl)
    end.

value(Value, #{convert_to_binary := true}) when is_list(Value) ->
    erlang:list_to_binary(Value);
value(Value, _) ->
    Value.

-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

insert_simple_test() ->
    A = new(),
    B = insert('_', "/my/profile/picture", "GET", "ONE", A),
    C = insert('_', "/my/inbox/:message", "POST", "TWO", B),
    D = insert('_', "/my/inbox/:message", "GET", "THREE", C),
    E = insert('_', "/my/inbox", "GET", "FOUR", D),
    Expected = #host_tree{hosts = [{'_', #routing_tree{tree =
                                                           [#node{segment = "my",value = [],
                                                                  children = [#node{segment = "inbox",
                                                                                    value = [#node_comp{comparator = "GET",value = "FOUR"}],
                                                                                    children = [#node{segment = "message",
                                                                                                      value = [#node_comp{comparator = "GET",value = "THREE"},
                                                                                                               #node_comp{comparator = "POST",value = "TWO"}],
                                                                                                      children = [],is_binding = true,is_wildcard = false}],
                                                                                    is_binding = false,is_wildcard = false},
                                                                              #node{segment = "profile",value = [],
                                                                                    children = [#node{segment = "picture",
                                                                                                      value = [#node_comp{comparator = "GET",value = "ONE"}],
                                                                                                      children = [],is_binding = false,is_wildcard = false}],
                                                                                    is_binding = false,is_wildcard = false}],
                                                                  is_binding = false,is_wildcard = false}]}}]},
    ?assertEqual(Expected, E).

insert_complex_test() ->
    A = new(),
    B = insert('_', "/profile/:id/picture", "GET", "ONE", A),
    C = insert('_', "/profile/:id", "GET", "TWO", B),
    D = insert('_', "/profile/:id", "POST", "THREE", C),
    Expected = #host_tree{hosts = [{'_',#routing_tree{tree = [#node{segment = "profile",value = [],
                                                                    children = [#node{segment = "id",
                                                                                      value = [#node_comp{comparator = "POST",value = "THREE"},
                                                                                               #node_comp{comparator = "GET",value = "TWO"}],
                                                                                      children = [#node{segment = "picture",
                                                                                                        value = [#node_comp{comparator = "GET",value = "ONE"}],
                                                                                                        children = [],is_binding = false,is_wildcard = false}],
                                                                                      is_binding = true,is_wildcard = false}],
                                                                    is_binding = false,is_wildcard = false}]}}],
                          options = #{convert_to_binary => false, use_strict => false}},
    ?assertEqual(Expected, D).

%% Expected to throw an exception since we are using strict mode
insert_duplicate_paths_throw_test() ->
    A = new(#{use_strict => true}),
    B = insert('_', "/profile", "GET", "ONE", A),
    ?assertException(throw, {duplicated_paths, _PathInfo, _Nodes}, insert('_', "/profile", "GET", "TWO", B)).


%% Expected the first route to stay the same - not overwriting.
insert_duplicate_paths_test() ->
    A = new(),
    B = insert('_', "/profile", "GET", "ONE", A),
    C = insert('_', "/profile", "GET", "TWO", B),
    Expected = #host_tree{hosts = [{'_', #routing_tree{tree = [#node{segment = "profile", value = [#node_comp{comparator = "GET", value = "ONE"}],
                                                                     children = []}]}}]},
    ?assertEqual(Expected, C).

insert_non_deterministic_path_test() ->
    A = new(#{use_strict => true}),
    B = insert('_', "/my/inbox/:message", "GET", "ONE", A),
    ?assertException(throw, {non_deterministic_paths, _PathInfo, _Node}, insert('_', "/my/inbox/1", "GET", "TWO", B)).

insert_non_deterministic_path2_test() ->
    A = new(#{use_strict => true}),
    B = insert('_', "/my/inbox/1", "GET", "ONE", A),
    ?assertException(throw, {non_deterministic_paths, _PathInfo, _Node}, insert('_', "/my/inbox/:message", "GET", "TWO", B)).

insert_wildcard_path_test() ->
    A = new(),
    B = insert('_', "/my/assets/[...]", "GET", "ONE", A),
    Expected = #host_tree{hosts = [{'_',#routing_tree{tree = [#node{segment = "my",
                                                                    value = [],
                                                                    children = [#node{segment = "assets",value = [],
                                                                                      children = [#node{segment = '...',
                                                                                                        value = [#node_comp{comparator = "GET",value = "ONE"}],
                                                                                                        children = [],is_binding = false,is_wildcard = true}],
                                                                                      is_binding = false,is_wildcard = false}],
                                                                    is_binding = false,is_wildcard = false}]}}],
                          options = #{use_strict => false, convert_to_binary => false}},
    ?assertEqual(Expected, B).

insert_wildcard_path_fail_test() ->
    ?assertException(throw, {bad_routingfile, _ErrorMsg}, insert('_', "/my/assets/[...]/not/working", "GET", "ONE", new())).

insert_disallowed_characters_test() ->
    ?assertException(throw, {unknown_symbol, "å"}, insert('_', "/my/åäö", "GET", "ONE", new())).

insert_binary_path_test() ->
    A = new(#{convert_to_binary => true, use_strict => false}),
    B = insert('_', "/profile", <<"GET">>, <<"ONE">>, A),
    Expected = #host_tree{hosts = [{'_',#routing_tree{tree = [#node{segment = <<"profile">>,
                                                                    value = [#node_comp{comparator = <<"GET">>,
                                                                                        value = <<"ONE">>}],
                                                                    children = [],is_binding = false,is_wildcard = false}]}}],
                          options = #{convert_to_binary => true,use_strict => false}},
    ?assertEqual(Expected, B).




-endif.
