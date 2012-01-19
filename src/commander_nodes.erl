%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_nodes.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Target nodes finding functions.
%%%----------------------------------------------------------------------------

-module(commander_nodes).
-export([get_nodes/1]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : get_nodes/1
%% Purpose  : Gets a list of nodes for the requested group.
%% Type     : {ok, list()} | {error, Reason}
%%-----------------------------------------------------------------------------
get_nodes(#nodes_opts{nodes=Nodes}) when Nodes /= "" ->
    {ok, string:tokens(Nodes, ",")};

get_nodes(#nodes_opts{nodes_group=pbs, try_all_nodes=MayBeTryAllNodes}) ->
    {ok, pbs_nodes(MayBeTryAllNodes)};

get_nodes(#nodes_opts{nodes_group=Group}) ->
    configured_groups(Group).


%%%============================================================================
%%% Private
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : configured_groups/1
%% Purpose  : Read and return a list of nodes from configured groups file.
%% Type     : {ok, list()} | {error, Reason}
%%-----------------------------------------------------------------------------
configured_groups(Group) ->
    configured_groups(file:read_file(?PATH_FILE__GROUPS), Group).


configured_groups({error, Reason}, _) ->
    {error, io_lib:format("COULD NOT OPEN CONFIG FILE: ~p", [Reason])};

configured_groups({ok, GroupsJsonBin}, Group) ->
    try ejson:decode(GroupsJsonBin) of
        {Groups} ->
            GroupBin = list_to_binary(atom_to_list(Group)),
            Nodes = proplists:get_value(GroupBin, Groups),

            case is_list(Nodes) of
                true ->
                    {ok, [binary_to_list(N) || N <- Nodes]};

                false ->
                    {error, io_lib:format("UNKNOWN NODES GROUP: ~s", [Group])}
            end
    catch
        _Exception ->
            {error, "COULD NOT DECODE JSON"}
    end.


%%-----------------------------------------------------------------------------
%% Function : pbs_nodes/1
%% Purpose  : Returns a list of TORQUE cluster nodes.
%% Type     : list(string())
%%-----------------------------------------------------------------------------
pbs_nodes(MayBeTryAllNodes) ->
    {Tree, _} = xmerl_scan:string(os:cmd("pbsnodes -x"), [{validation, off}]),
    ListOfNodeTrees = nth_of_tuple(9, Tree),
    ListOfNodeData  = [pbs_node_data(Node) || Node <- ListOfNodeTrees],
    ListOfNodeNames = [
        Node#node_data.name || Node <- ListOfNodeData,
        case MayBeTryAllNodes of
            true -> true;
            false -> is_node_available(Node#node_data.states)
        end
    ],
    ListOfNodeNames.


%%-----------------------------------------------------------------------------
%% Function : pbs_node_data/1 -> pbs_node_data/2
%% Purpose  : Returns a tuple with a nodes name and state.
%% Type     : #node_data{}
%%-----------------------------------------------------------------------------
pbs_node_data(Node) ->
    Data = nth_of_tuple(9, Node),
    pbs_node_data(Data, #node_data{}).


pbs_node_data([], NodeData) -> NodeData;

pbs_node_data([Data|DataTail], NodeData) ->
    [Datum] = nth_of_tuple(9, Data),

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
%% Function : is_node_available/1
%% Purpose  : Checks whether a given node's state is considered available.
%% Type     : bool()
%%-----------------------------------------------------------------------------
is_node_available([]) -> true;

is_node_available([State|StatesTail]) ->
    case lists:member(State, ?UNAVAILABLE_STATES) of
        true -> false;
        false -> is_node_available(StatesTail)
    end.


%%-----------------------------------------------------------------------------
%% Function : nth_of_tuple/2
%% Purpose  : Returns an Nth element of a tuple. Just a shortcut.
%% Type     : any()
%%-----------------------------------------------------------------------------
nth_of_tuple(N, Tuple) ->
    lists:nth(N, tuple_to_list(Tuple)).
