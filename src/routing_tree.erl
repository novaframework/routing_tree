-module(routing_tree).
-export([
         new/0,
         new/1,
         lookup/3,
         insert/3
        ]).

-include("../include/routing_tree.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Creates a new routing tree structure with options set to <icode>Options</icode>.
%% @end
%%--------------------------------------------------------------------
-spec new(Options :: routing_tree_options()) -> #routing_tree{}.
new(Options) when is_map(Options) ->
    #routing_tree{options = Options}.


%%--------------------------------------------------------------------
%% @doc
%% Creates a new routing tree structure. Same as new/1 with Options set
%% to <icode>#{use_strict => false}</icode>
%% @end
%%--------------------------------------------------------------------
-spec new() -> #routing_tree{}.
new() ->
    new(#{use_strict => false}).



%%--------------------------------------------------------------------
%% @doc
%% Makes a lookup in the database for a specific path and method
%% @end
%%--------------------------------------------------------------------
-spec lookup(Path :: binary() | integer(), Method :: binary(), RoutingTree :: #routing_tree{}) -> {ok, ok} | %% Todo! Fix spec
                                                                                                  {error, Reason :: any()}.
lookup(Path, Method, #routing_tree{tree = Tree, options = Options}) when is_integer(Path) orelse
                                                                         is_binary(Path) ->
    lookup(Path, Method, <<>>, Tree, Options#{bindings => #{}}).

%%--------------------------------------------------------------------
%% @doc
%% Lookup a url in the dispatch table.
%% @end
%%--------------------------------------------------------------------
-spec lookup(Path :: binary(), Method :: binary(), Acc :: binary(), PreviousNode :: #node{}, State :: map()) ->
                    {ok, Value :: #node_value{}, State0 :: map()} |
                    {error, Reason :: any()}.
lookup(<<>>, Method, Acc, #node{children = Children}, #{bindings := Bindings} = State) ->
    case find_node(Acc, Method, Children, []) of
        {ok, binding, {#node{path = Key}, Value}} ->
            {ok, Value, State#{bindings => Bindings#{Key => Acc}}};
        {ok, _, {_Node, Value}} ->
            {ok, Value, Bindings};
        Error -> Error
    end;
lookup(<<$/, Rest/binary>>, Method, <<>>, Tree, Options) ->
    %% Ignore since double /
    lookup(Rest, Method, <<>>, Tree, Options);
lookup(<<$/, Rest/binary>>, Method, Acc, #node{children = Children}, #{bindings := Bindings} = State) ->
    case find_node(Acc, false, Children, []) of
        {ok, path, {Node, _Value}} ->
            lookup(Rest, Method, <<>>, Node, State);
        {ok, wildcard, {_, Value}} ->
            {ok, Value, Bindings};
        {ok, binding, {#node{path = Key}=Node, undefined}} ->
            lookup(Rest, Method, <<>>, Node, State#{bindings => Bindings#{Key => Acc}});
        Error -> Error
    end;
lookup(<<X, Rest/binary>>, Method, Acc, Tree, State) ->
    lookup(Rest, Method, <<Acc/binary, X>>, Tree, State).



%%--------------------------------------------------------------------
%% @doc
%% Insert a new node in the tree
%% @end
%%--------------------------------------------------------------------
insert(Path, Value, #routing_tree{tree = T, options = Options}) ->
    insert(Path, <<>>, Value, T, Options).

insert(Path, _Acc, Value, #node{children = Children}, _Options) when not is_binary(Path) ->
    case lists:keyfind(Path, #node.path, Children) of
        false ->
            #node{path = Path,
                  value = [Value]};
        _ ->
            {warning, route_already_defined}
    end;
insert(<<>>, Acc, Value, #node{children = Children}, _Options) ->
    case lists:keyfind(Acc, #node.path, Children) of
        false ->
            #node{path = Acc,
                  value = [Value]};
        Result ->
            %% We need to check all the methods of the results with all the methods given my this path
            case find_method(Value#node_value.method, [Result]) of
                {error, not_found} ->
                    Result#node{value = [Value|Result#node.value]};
                _Node ->
                    %% We have a conflict
                    {error, duplicated_paths}
            end
    end;
insert(<<$/, Rest/binary>>, <<>>, Value, PrevNode, Options) ->
    %% Ignore since this was either the root or a double /
    insert(Rest, <<>>, Value, PrevNode, Options);
insert(<<$/, Rest/binary>>, Acc, Value, #node{children = Children}, Options) ->
    case has_bindings(Children) of
        false ->
            case lists:keyfind(Acc, #node.path, Children) of
                false ->
                    #node{path = Acc,
                          children = [insert(Rest, <<>>, Value, new(), Options)]};
                Subtree ->
                    %% Already exist so just go down in tree
                    Child = insert(Rest, <<>>, Value, Subtree, Options),
                    Subtree#node{children = [Child|without_child(Child, Subtree#node.children)]}
            end;
        _ ->
            {warning, binding_overlapping_route}
    end;
insert(<<$:, Rest/binary>>, _Acc, Value, #node{children = Children}, Options) ->
    %% No worries here, just continue
    case parse_binding(Rest, <<>>) of
        {Binding, <<>>} ->
            case lists:search(fun(#node{path = Key, is_binding = true}) when Key =:= Binding -> true;
                                 (_) -> false end, Children) of
                false ->
                    #node{path = Binding,
                          is_binding = true,
                          value = [Value]};
                {value, Element} ->
                    Element#node{is_binding = true,
                                 value = [Value|Element#node.value]}
            end;
        {Binding, Rest0} ->
            case lists:search(fun(#node{path = Key, is_binding = true}) when Key =:= Binding -> true;
                                 (_) -> false end, Children) of
                false ->
                    #node{path = Binding,
                          is_binding = true,
                          children = [insert(Rest0, <<>>, Value, new(), Options)]};
                {value, Element} ->
                    Child = insert(Rest0, <<>>, Value, Element, Options),
                    Element#node{path = Binding, is_binding = true, children = [Child|without_child(Child, Element#node.children)]}
            end
    end;
insert(<<$[, $., $., $., $]>>, _Acc, Value, _PrevNode, _Options) ->
    %% This is a catch-all-route - snould be the last thing in a route
    #node{path = '_', is_binding = false, value = [Value]};
insert(<<X, Rest/bits>>, Acc, Value, PrevNode, Options) ->
    insert(Rest, <<Acc/binary, X>>, Value, PrevNode, Options).


%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%


has_bindings([]) -> false;
has_bindings([#node{is_binding = true}|_Tl]) -> true;
has_bindings([_|Tl]) -> has_bindings(Tl).

find_node(_, _, [], []) -> {error, {not_found, path}};
find_node(_, _, [], Binding) -> {ok, binding, Binding};
find_node(Key, false, [#node{is_binding = true} = N|Children], _Binding) ->
    find_node(Key, false, Children, {N, undefined});
find_node(Key, Method, [#node{path = '_', value = Values}|Children], Binding) ->
    case find_method(Method, Values) of
        {error, not_found} -> find_node(Key, Method, Children, Binding);
        {Node, Value} -> {ok, wildcard, {Node, Value}}
    end;
find_node(Key, Method, [#node{is_binding = true, value = Values}|Children], Binding) ->
    case find_method(Method, Values) of
        {error, not_found} -> find_node(Key, Method, Children, Binding);
        {Node, Value} -> find_node(Key, Method, Children, {Node, Value})
    end;
find_node(Key, false, [#node{path = Key} = N|_Children], _Binding) ->
    {ok, path, {N, undefined}};
find_node(Key, Method, [#node{path = Key, value = Values}|_Children], _Binding) ->
    case find_method(Method, Values) of
        {error, not_found} -> {error, {not_found, method}};
        {Node, Value} -> {ok, path, {Node, Value}}
    end;
find_node(Key, Method, [_Hd|Tl], Bindings) ->
    find_node(Key, Method, Tl, Bindings).

find_method(_Method, []) -> {error, not_found};
find_method('_', [Elem|_Tl]) ->
    {undefined, Elem};
find_method(Method, [#node_value{method = Method} = Value|_Tl]) ->
    {undefined, Value};
find_method(_, [#node_value{method = '_'} = Value|_Tl]) ->
    {undefined, Value};
find_method(Method, [_|Tl]) ->
    find_method(Method, Tl).


without_child(_Node, []) -> [];
without_child(#node{path = Key}, [#node{path = Key}|Children]) ->
    Children;
without_child(Node, [Hd|Tl]) ->
    [Hd|without_child(Node, Tl)].


parse_binding(<<>>, Acc) ->
    {Acc, <<>>};
parse_binding(<<$/, Rest/bits>>, Acc) ->
    {Acc, <<Rest/binary>>};
parse_binding(<<C, Rest/bits>>, Acc) ->
    parse_binding(Rest, << Acc/binary, C >>).
