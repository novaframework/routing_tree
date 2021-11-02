-module(routing_tree).
-export([
         new/0,
         new/1,
         insert/5,
         lookup/4
        ]).

-include("../include/routing_tree.hrl").


%%========================================
%% API
%%========================================

-spec new() -> #host_tree{}.
new() ->
    #host_tree{}.

-spec new(Options :: options()) -> #host_tree{}.
new(Options) ->
    #host_tree{options = #{use_strict => maps:get(use_strict, Options, false),
                           convert_to_binary => maps:get(convert_to_binary, Options, false)}}.

-spec lookup(Host :: binary() | '_', Path :: list() | integer() | binary(), Comparator :: any(), #host_tree{}) -> {ok, Bindings :: map(), Value :: any()} |
                                                                                                       {ok, Bindings :: map(), Value :: any(), PathInfo :: [binary()]} |
                                                                                                       {error, Reason :: term()}.
lookup(Host, Path, Comparator, Hosts) when is_integer(Path) ->
    lookup(Host, [Path], Comparator, Hosts);
lookup(Host, Path, Comparator, #host_tree{hosts = Hosts}) ->
    case lists:keyfind(Host, 1, Hosts) of
        false ->
            case lists:keyfind('_', 1, Hosts) of
                false ->
                    {error, not_found};
                {_, #routing_tree{tree = Tree}} ->
                    lookup_path(Path, Comparator, Tree)
            end;
        {_, #routing_tree{tree = Tree}} ->
            lookup_path(Path, Comparator, Tree)
    end.

-spec lookup_path(Segments :: [string()] | binary(), Comparator :: any(), Tree :: [#node{}]) ->
                         {ok, Bindings :: map(), Value :: any()} |
                         {ok, Bindings :: map(), Value :: any(), PathInfo :: [binary()]} |
                         {error, not_found}.
lookup_path(Path, Comparator, Tree) when is_binary(Path) ->
    lookup_binary(Path, Comparator, Tree, {#{}, undefined}, <<>>);
lookup_path(Path, Comparator, Tree) when is_list(Path) ->
    lookup_path(Path, Comparator, Tree, {#{}, undefined}).

-spec lookup_path([Segments :: list()], Comparator :: any(), Tree :: [#node{}], {Bindings :: map(), Node :: #node{} | undefined}) ->
                         {ok, Bindings :: map(), Value :: any()} |
                         {error, not_found}.
lookup_path([], _Comparator, _, {_, undefined}) ->
    {error, not_found};
lookup_path([], Comparator, _, {Bindings, Node}) ->
    case find_comparator(Comparator, Node#node.value) of
        {ok, #node_comp{value = Value}} ->
            {ok, Bindings, Value};
        Error ->
            Error
    end;
lookup_path([Segment|Tl], Comparator, Tree, {Bindings, _}) ->
    case lookup_segment(Segment, Bindings, Tree) of
        {error, not_found} ->
            {error, not_found};
        {ok, Bindings0, #node{children = Children} = N} ->
            %% This is a bit special since we need to bind the values to this and continue
            lookup_path(Tl, Comparator, Children, {Bindings0, N})
    end.


lookup_binary(<<>>, Comparator, Tree, {Bindings, _Node}, Ack) ->
    case lookup_segment(Ack, Bindings, Tree) of
        {ok, Bindings0, #node{is_wildcard = Wildcard, value = Value}} ->
            case find_comparator(Comparator, Value) of
                {ok, #node_comp{value = Value0}} ->
                    case Wildcard of
                        false ->
                            {ok, Bindings0, Value0};
                        _ ->
                            {ok, Bindings0, Value0, [Ack]}
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
lookup_binary(<<$/, Rest/bits>>, Comparator, Tree, Bindings, <<>>) ->
    %% Double // - just continue
    lookup_binary(Rest, Comparator, Tree, Bindings, <<>>);
lookup_binary(<<$/, Rest/bits>>, Comparator, Tree, {Bindings, _}, Ack) ->
    case lookup_segment(Ack, Bindings, Tree) of
        {ok, Bindings0, #node{is_wildcard = true, value = Value}} ->
            %% We need to handle this special case
            case find_comparator(Comparator, Value) of
                {ok, #node_comp{value = Value0}} ->
                    Tokens = binary:split(Rest, <<"/">>, [global, trim_all]),
                    {ok, Bindings0, Value0, [Ack | Tokens]};
                _ ->
                    {error, not_found}
            end;
        {ok, Bindings0, SubNode = #node{children = Children}} ->
            lookup_binary(Rest, Comparator, Children, {Bindings0, SubNode}, <<>>);
        Error ->
            Error
    end;
lookup_binary(<<$?, _Rest/bits>>, Comparator, Tree, Bindings, Ack) ->
    %% TODO! Implement query parameters parsing
    lookup_binary(<<>>, Comparator, Tree, Bindings, Ack);
lookup_binary(<<$#, _Rest/bits>>, Comparator, Tree, Bindings, Ack) ->
    %% TODO! Implement fragment parsing
    lookup_binary(<<>>, Comparator, Tree, Bindings, Ack);
lookup_binary(<<Char, Rest/bits>>, Comparator, Tree, Bindings, Ack) ->
    lookup_binary(Rest, Comparator, Tree, Bindings, << Ack/binary, Char >>).




-spec insert(Host :: any(), Path :: list() | integer(), Comparator :: any(), Value :: any(), HT :: #host_tree{}) -> #host_tree{}.
insert(Host, StatusCode, _Comparator, Value, #host_tree{hosts = Hosts} = HT) when is_integer(StatusCode) ->
    %% We don't need to tokenize since it's a integer
    RT =
        case lists:keyfind(Host, 1, Hosts) of
            false -> #routing_tree{};
            {_, RoutingTree} -> RoutingTree
        end,
    case lists:keyfind(StatusCode, #node.segment, RT#routing_tree.tree) of
        false ->
            %% Just append to the tree
            RT0 = RT#routing_tree{tree = [#node{segment = StatusCode, value = [#node_comp{comparator = '_', value = Value}]}|RT#routing_tree.tree]},
            HT#host_tree{hosts = [{Host, RT0}|lists:keydelete(Host, 1, Hosts)]};
        #node{value = _Values} ->
            %% TODO! Check if there is duplicate comparators or if we can insert this entry
            HT
    end;
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

-spec insert([{Type :: atom(), Line :: number(), Ident :: list()}], CompNode :: #node_comp{}, Siblings :: [#node{}], Options :: options()) ->
                    Tree :: [#node{}].
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
                                DupNodes ->
                                    throw({duplicated_paths, {Ident, CompNode}, DupNodes})
                            end
                    end;
                _ ->
                    [Node#node{children = insert(Tl, CompNode, Node#node.children, Options),
                               is_binding = Type == binding, is_wildcard = Type == wildcard} | lists:delete(Node, Siblings)]
            end
    end.


%%========================================
%% Private functions
%%========================================


lookup_segment(Segment, Bindings, Tree) ->
    lookup_segment(Segment, Bindings, Tree, undefined).

lookup_segment(_Ident, _Bindings, [], undefined) ->
    {error, not_found};
lookup_segment(_Ident, Bindings, [], WCNode) ->
    {ok, Bindings, WCNode};
lookup_segment(Ident, Bindings, [#node{segment = Ident, is_binding = false,
                                                    is_wildcard = false} = N|_Tl], _WCNode) ->
    {ok, Bindings, N};
lookup_segment(Ident, Bindings, [#node{segment = Segment, is_binding = true} = N|_Tl], _WCNode) ->
    {ok, Bindings#{Segment => Ident}, N};
lookup_segment(Ident, Bindings, [#node{is_wildcard = true} = N|Tl], _WCNode) ->
    lookup_segment(Ident, Bindings, Tl, N);
lookup_segment(Ident, Bindings, [_Hd|Tl], WCNode) ->
    lookup_segment(Ident, Bindings, Tl, WCNode).


-spec find_comparator(Comparator :: any(), [#node_comp{}]) -> {ok, Node :: #node_comp{}} |
                                                              {error, not_found}.
find_comparator(_, []) -> {error, not_found};
find_comparator(Comparator, [#node_comp{comparator = Comparator}=Node|_Tl]) ->
    {ok, Node};
find_comparator(Comparator, [#node_comp{comparator = '_'}=Node|Tl]) ->
    case find_comparator(Comparator, Tl) of
        {ok, Node0} ->
            {ok, Node0};
        _ ->
            {ok, Node}
    end;
find_comparator(Comparator, [_Node|Tl]) ->
    find_comparator(Comparator, Tl).

check_conflicting_nodes(_, _, _, _, []) -> false;
check_conflicting_nodes(segment, Ident, [] = Tl, CompNode, Siblings) ->
    first(fun(#node{value = Value, segment = Segment, is_binding = false, is_wildcard = false} = Node) ->
                  {[ C || #node_comp{comparator = C} <- Value,
                          C == CompNode#node_comp.comparator ] /= [] andalso Segment == Ident andalso Tl /= [], {duplicate, Node}};
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


%%========================================
%% EUnit tests
%%========================================
-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

insert_simple_test() ->
    A = new(),
    B = insert('_', "/my/profile/picture", "GET", "ONE", A),
    C = insert('_', "/my/inbox/:message", "POST", "TWO", B),
    D = insert('_', "/my/inbox/:message", "GET", "THREE", C),
    E = insert('_', "/my/inbox", "GET", "FOUR", D),
    F = insert('_', "/", "GET", "FIVE", E),
    Expected = #host_tree{hosts = [{'_',#routing_tree{tree = [#node{segment = [],
                                                                    value = [#node_comp{comparator = "GET",value = "FIVE"}],
                                                                    children = [],is_binding = false,is_wildcard = false},
                                                              #node{segment = "my",value = [],
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
                                                                    is_binding = false,is_wildcard = false}]}}],
                          options = #{convert_to_binary => false,use_strict => false}},
    ?assertEqual(Expected, F).

insert_simple2_test() ->
    A = new(#{convert_to_binary => true}),
    B = insert('_', "/inbox", "GET", "ONE", A),
    C = insert('_', "/inbox/message", "GET", "ONE", B),
    Expected = #host_tree{hosts = [{'_',#routing_tree{tree = [#node{segment = <<"inbox">>,
                                                                    value = [#node_comp{comparator = "GET",value = "ONE"}],
                                                                    children = [#node{segment = <<"message">>,
                                                                                      value = [#node_comp{comparator = "GET",value = "ONE"}],
                                                                                      children = [],is_binding = false,is_wildcard = false}],
                                                                    is_binding = false,is_wildcard = false}]}}],
                          options = #{convert_to_binary => true,use_strict => false}},
    ?assertEqual(Expected, C).

insert_wildcard_test() ->
    A = new(),
    B = insert('_', "/inbox/[...]", "GET", "ONE", A),
    Expected = #host_tree{hosts = [{'_',#routing_tree{tree = [#node{segment = "inbox",
                                                                    value = [],
                                                                    children = [#node{segment = '...',
                                                                                      value = [#node_comp{comparator = "GET",value = "ONE"}],
                                                                                      children = [],is_binding = false,is_wildcard = true}],
                                                                    is_binding = false,is_wildcard = false}]}}],
                          options = #{convert_to_binary => false,use_strict => false}},
    ?assertEqual(Expected, B).

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

simple_string_list_lookup_test() ->
    A = new(),
    B = insert('_', "/my/route", "GET", "ONE", A),
    C = lookup(<<"My host">>, ["my", "route"], "GET", B),
    Expected = {ok, #{}, "ONE"},
    ?assertEqual(Expected, C).

simple_binary_list_lookup_test() ->
    A = new(#{convert_to_binary => true}),
    B = insert('_', "/my/route", "GET", "ONE", A),
    C = lookup(<<"My host">>, [<<"my">>, <<"route">>], "GET", B),
    Expected = {ok, #{}, "ONE"},
    ?assertEqual(Expected, C).


simple_binary_lookup_test() ->
    A = new(#{convert_to_binary => true}),
    B = insert('_', "/my/route", "GET", "ONE", A),
    C = lookup(<<"My host">>, <<"/my/route">>, "GET", B),
    Expected = {ok, #{}, "ONE"},
    ?assertEqual(Expected, C).


bindings_binary_lookup_test() ->
    A = new(#{convert_to_binary => true}),
    B = insert('_', "/my/:route", "GET", "ONE", A),
    C = lookup(<<"My host">>, <<"/my/monkey">>, "GET", B),
    Expected = {ok, #{<<"route">> => <<"monkey">>}, "ONE"},
    ?assertEqual(Expected, C).

complex_binary_lookup_test() ->
    A = new(#{convert_to_binary => true}),
    B = insert('_', "/my/:route", "GET", "ONE", A),
    C = insert('_', "/my/inbox/:message", "POST", "TWO", B),
    D = insert('_', "/my/inbox/:message", "GET", "THREE", C),
    E = insert('_', "/my/inbox", "GET", "FOUR", D),
    F = insert('_', "/", "GET", "FIVE", E),

    G = lookup(<<"My host">>, <<"/my/inbox/hello">>, "GET", F),
    Expected = {ok, #{<<"message">> => <<"hello">>}, "THREE"},
    ?assertEqual(Expected, G).

comparator_binary_lookup_test() ->
    A = new(#{convert_to_binary => true}),
    B = insert('_', "/my/route", '_', "ONE", A),
    C = lookup(<<"My host">>, <<"/my/route">>, "PUT", B),
    Expected = {ok, #{}, "ONE"},
    ?assertEqual(Expected, C).


wildcard_binary_lookup_test() ->
    A = new(#{convert_to_binary => true}),
    B = insert('_', "/my/route/[...]", '_', "ONE", A),
    C = lookup(<<"My host">>, <<"/my/route/is/amazing">>, "PUT", B),
    Expected = {ok, #{}, "ONE", [<<"is">>, <<"amazing">>]},
    ?assertEqual(Expected, C).




-endif.
