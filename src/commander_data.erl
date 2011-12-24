%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_data.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Data-gathering functions.
%%%----------------------------------------------------------------------------

-module(commander_data).
-export([pbs_nodes/1]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : pbs_nodes/1
%% Purpose  : Returns a list of TORQUE cluster nodes and their states.
%% Type     : list(#node_data{})
%%-----------------------------------------------------------------------------
pbs_nodes(MayBeTryAllNodes) ->
    {Tree, _} = xmerl_scan:string(os:cmd("pbsnodes -x"), [{validation, off}]),
    NodesTree = commander_lib:nth_of_tuple(9, Tree),
    NodesData = [pbs_node_data(Node) || Node <- NodesTree],
    NodeNames = [
        Node#node_data.name || Node <- NodesData,
        case MayBeTryAllNodes of
            true -> true;
            false -> node_available(Node#node_data.states)
        end
    ],
    NodeNames.


%%%============================================================================
%%% Private
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : pbs_node_data/1 -> pbs_node_data/2
%% Purpose  : Returns a tuple with a nodes name and state.
%% Type     : #node_data{}
%%-----------------------------------------------------------------------------
pbs_node_data(Node) ->
    Data = commander_lib:nth_of_tuple(9, Node),
    pbs_node_data(Data, #node_data{}).


pbs_node_data([], NodeData) -> NodeData;

pbs_node_data([Data|DataTail], NodeData) ->
    [Datum] = commander_lib:nth_of_tuple(9, Data),

    case Datum of
        {xmlText, [{name, _}, _, _], _, _, Name, text} ->
            pbs_node_data(DataTail, NodeData#node_data{name = Name});

        {xmlText, [{state, _}, _, _], _, _, StatesString, text} ->
            States = string:tokens(StatesString, ","),
            pbs_node_data(DataTail, NodeData#node_data{states = States});

        _Else ->
            pbs_node_data(DataTail, NodeData)
    end.


%%-----------------------------------------------------------------------------
%% Function : node_available/1
%% Purpose  : Checks whether a given node's state is considered available.
%% Type     : bool()
%%-----------------------------------------------------------------------------
node_available([]) -> true;

node_available([State|StatesTail]) ->
    case lists:member(State, ?UNAVAILABLE_STATES) of
        true -> false;
        false -> node_available(StatesTail)
    end.
