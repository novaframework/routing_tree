%%%-------------------------------------------------------------------
%% @doc routing_tree public API
%% @end
%%%-------------------------------------------------------------------

-module(routing_tree_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    routing_tree_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
