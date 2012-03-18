%%%----------------------------------------------------------------------------
%%% @author Siraaj Khandkar <siraaj@khandkar.net>
%%%  [http://ibnfirnas.github.com]
%%% @copyright 2011-2012 Siraaj Khandkar
%%% @doc Target nodes finding functions.
%%% @end
%%%----------------------------------------------------------------------------

-module(commander_nodes).
-export([get_nodes/1]).


-include("commander_config.hrl").
-include("commander_records.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : get_nodes/1
%% Purpose  : Gets a list of nodes for the requested group.
%% Type     : {ok, list()} | {error, Reason}
%%-----------------------------------------------------------------------------
get_nodes(#nodes_opts{nodes=Nodes}=Options) when Nodes /= "" ->
    Pattern = Options#nodes_opts.filter_pattern,
    get_nodes(filter, {ok, string:tokens(Nodes, ",")}, Pattern);

get_nodes(#nodes_opts{nodes_group=pbs}=Options) ->
    Pattern = Options#nodes_opts.filter_pattern,
    MayBeTryAllNodes = Options#nodes_opts.try_all_nodes,
    get_nodes(filter, pbs_nodes(MayBeTryAllNodes), Pattern);

get_nodes(#nodes_opts{nodes_group=Group}=Options) ->
    Pattern = Options#nodes_opts.filter_pattern,
    get_nodes(filter, configured_groups(Group), Pattern).


get_nodes(filter, {ok, Nodes}, Pattern) ->
    MatchedNodes = [N || N <- Nodes, commander_lib:is_match(N, Pattern)],
    {ok, MatchedNodes};

get_nodes(filter, {error, Reason}, _) -> {error, Reason}.


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
        {ErrorType, _ErrorDetails} ->
            {error, io_lib:format("COULD NOT DECODE JSON: ~p", [ErrorType])}
    end.


%%-----------------------------------------------------------------------------
%% Function : pbs_nodes/1
%% Purpose  : Returns a list of TORQUE cluster nodes.
%% Type     : list(string())
%%-----------------------------------------------------------------------------
pbs_nodes(MayBeTryAllNodes) ->
    pbs_nodes(commander_lib:os_cmd("pbsnodes -x"), MayBeTryAllNodes).

pbs_nodes({error, Reason     }, _               ) -> {error, Reason};
pbs_nodes({ok,    PBSNodesXML}, MayBeTryAllNodes) ->
    {Tree, _} = xmerl_scan:string(PBSNodesXML, [{validation, off}]),
    ListOfNodeTrees = element(9, Tree),
    ListOfNodeData  = [pbs_node_data(Node) || Node <- ListOfNodeTrees],
    ListOfNodeNames = [
        Node#node_data.name || Node <- ListOfNodeData,
        case MayBeTryAllNodes of
            true -> true;
            false -> is_node_available(Node#node_data.states)
        end
    ],
    {ok, ListOfNodeNames}.


%%-----------------------------------------------------------------------------
%% Function : pbs_node_data/1 -> pbs_node_data/2
%% Purpose  : Returns a tuple with a nodes name and state.
%% Type     : #node_data{}
%%-----------------------------------------------------------------------------
pbs_node_data(Node) ->
    Data = element(9, Node),
    pbs_node_data(Data, #node_data{}).


pbs_node_data([], NodeData) -> NodeData;

pbs_node_data([Data|DataTail], NodeData) ->
    [Datum] = element(9, Data),

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


%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).


is_node_available_test() ->
    TrueCases  = [
        ["busy"],
        ["free"],
        ["job-exclusive"],
        ["job-sharing"],
        ["reserve"],
        ["state-unknown"],
        ["time-shared"]
    ],

    FalseCases = [
        ["down"],
        ["offline"]
    ],

    lists:foreach(
        fun(Case) ->
            true = is_node_available(Case)
        end,
        TrueCases
    ),

    lists:foreach(
        fun(Case) ->
            false = is_node_available(Case)
        end,
        FalseCases
    ).

-endif.
