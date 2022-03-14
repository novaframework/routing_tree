-module(routing_tree_binparser).
-export([
         parse/1
        ]).

parse(Segment) ->
    parse(Segment, <<>>, segment).


%% There's nothing else after query parameters
parse(<<>>, Acc, Type) ->
    [{Type, Acc}];
parse(<<$?, Query/bits>>, Acc, Type) ->
    [{Type, Acc}, {query, parse_query(Query, <<>>, [])}];
parse(<<$/, Rest/bits>>, <<>>, _Type) ->
    %% This could be caused by double / or root level
    parse(Rest, <<>>, segment);
parse(<<$:, Rest/bits>>, <<>>, _Type) ->
    %% This is a binding
    parse(Rest, <<>>, binding);
parse(<<$/, Rest/bits>>, Acc, Type) ->
    %% We have an acc
    [{Type, Acc}|parse(Rest, <<>>, segment)];
parse(<<$[, $., $., $., $]>>, <<>>, _Type) ->
    %% Wildcard - We can just return this
    [{wildcard, '...'}];
parse(<<$[, $., $., $., $], _Rest/bits>>, <<>>, _Type) ->
    throw({bad_routingfile, wildcard_not_last_in_path});
parse(<<C, Rest/bits>>, Acc, Type) ->
    parse(Rest, << Acc/binary, C >>, Type).


%% Parse on format x=w&y=z
parse_query(<<>>, Acc, [Hd|Tl]) ->
    [{Hd, Acc}|Tl];
parse_query(<<$=, Rest/bits>>, Acc, List) ->
    parse_query(Rest, <<>>, [Acc|List]);
parse_query(<<$&, Rest/bits>>, Acc, [Hd|Tl]) ->
    parse_query(Rest, <<>>, [{Hd, Acc}|Tl]);
parse_query(<<C, Rest/bits>>, Acc, List) ->
    parse_query(Rest, <<Acc/binary, C>>, List).



%%========================================
%% EUnit tests
%%========================================
-ifdef(TEST).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

basic_parse_test() ->
    Expected = [{segment, <<"my">>}, {segment, <<"route">>}],
    ?assertEqual(Expected, parse(<<"/my/route">>)).

binding_in_path_test() ->
    Expected = [{segment, <<"my">>}, {binding, <<"binding">>}],
    ?assertEqual(Expected, parse(<<"/my/:binding">>)).

query_test() ->
    Expected = [{segment, <<"my">>}, {query, [{<<"query">>, <<"1">>}]}],
    ?assertEqual(Expected, parse(<<"/my?query=1">>)).

binding_and_query_test() ->
    Expected = [{segment, <<"my">>}, {binding, <<"binding">>}, {query, [{<<"hello">>, <<"world">>}]}],
    ?assertEqual(Expected, parse(<<"/my/:binding?hello=world">>)).


-endif.
