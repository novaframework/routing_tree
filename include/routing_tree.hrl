-record(path_info, {
                    value = [] :: any(),
                    qs = [] :: [{binary(), binary() | true}],
                    bindings = [] :: [{binary(), binary()}]
                   }).

-type path_info() :: #path_info{}.

-record(rt_node, {
                  segment = <<>> :: binary(),
                  value = [] :: any(),
                  comparator = <<>> ::  binary(),
                  children = []
              }).

-type rt_node() :: #rt_node{}.

-export_type([
              path_info/0,
              rt_node/0
             ]).
