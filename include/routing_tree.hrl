%% Method, Module, Function-record
-type options() :: #{
                                  use_strict := boolean()
                                 }.
-export_type([options/0]).

-record(node_comp, {
                    comparator = '_' :: '_' | binary(),
                    value = undefined :: any()
                   }).

-record(node, {
               segment = <<>> :: binary() | '_',
               value = [] :: [#node_comp{}],
               children = [] :: [#node{}],
               is_binding = false :: boolean(), %% Indicates if this path has any bindings (Eg /:binding)
               is_wildcard = false :: boolean() %% Indicates if this is a '...'-path
              }).

-record(routing_tree, {
                       tree = [] :: [#node{}]
                      }).

-record(host_tree, {
                    hosts = [] :: [{Host :: binary() | '_', Tree :: #routing_tree{}}],
                    options = #{
                                use_strict => false,
                                convert_to_binary => false
                               } :: options()
                   }).
