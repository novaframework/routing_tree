%% Method, Module, Function-record

-record(node_value, {
                     method = '_' :: binary() | '_',
                     value :: any()
                    }).


-record(node, {
               path = <<>> :: binary() | '_',
               value = [] :: [#node_value{}],
               children = [] :: [#node{}],
               is_binding = false :: boolean(), %% Indicates if this path has any bindings (Eg /:binding)
               is_wildcard = false :: boolean() %% Indicates if this is a '...'-path
              }).

-type routing_tree_options() :: #{
                                  use_strict := boolean()
                                 }.
-export_type([routing_tree_options/0]).

-record(routing_tree, {
                       tree = undefined :: #node{} | undefined,
                       %% Some properties for this tree
                       options = #{
                                   use_strict => false
                                  } :: routing_tree_options()
                      }).
