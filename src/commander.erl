%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Executes a command on all available cluster nodes.
%%%----------------------------------------------------------------------------

-module(commander).
-export([main/1, executor/0, pbs_nodes/0]).


-include("commander_config.hrl").


main(Args) ->
    [Host, User, Command] = lists:map(fun(A) -> atom_to_list(A) end, Args),

    crypto:start(),
    ssh:start(),

    register(executor_proc, spawn(commander, executor, [])),

    executor_proc ! {job, {Host, User, Command}}.


executor() ->
    receive
        {job, {Host, User, Command}} ->
            ConnectOptions = [
                {silently_accept_hosts, true},
                {user_interaction, true},
                {connect_timeout, ?TIMEOUT},
                {user, User},
                {user_dir, ?PATH_DIR__DATA_SSH}
            ],

            {ok, ConnRef} = ssh:connect(Host, ?PORT, ConnectOptions),
            {ok, ChannId} = ssh_connection:session_channel(ConnRef, ?TIMEOUT),

            ssh_connection:exec(ConnRef, ChannId, Command, ?TIMEOUT),
            executor();

        {ssh_cm, ConnRef, {data, _, _, Data}} ->
            NodeId = pid_to_list(ConnRef),
            NodeOutput = binary_to_list(Data),

            StdOutput =
                string:join(["\n", NodeId, ?SEPARATOR, NodeOutput], "\n"),

            io:format(StdOutput),

            executor_proc ! stop,
            executor();

        {ssh_cm, _, {eof, _}} -> executor();
        {ssh_cm, _, {exit_status, _, _}} -> executor();
        {ssh_cm, _, {closed, _}} -> executor();

        stop ->
            init:stop();

        Other ->
            io:format("WARNING! UNEXPECTED MSG: ~n~p~n", [Other]),
            executor()
    end.


%%-----------------------------------------------------------------------------
%% Function : pbs_nodes/0
%% Purpose  : Returns a list of TORQUE cluster nodes and their states.
%%-----------------------------------------------------------------------------
pbs_nodes() ->
    {Tree, _} = xmerl_scan:string(os:cmd("pbsnodes -x"), [{validation, off}]),
    {_, _, _, _, _, _, _, _, Nodes, _, _, _} = Tree,
    PBSNodes = [pbs_node_data(Node) || Node <- Nodes],
    PBSNodes.


pbs_node_data(Node) ->
    {_, _, _, _, _, _, _, _, Data, _, _, _} = Node,
    pbs_node_data(Data, []).


pbs_node_data([], ExtractedData) ->
    list_to_tuple(ExtractedData);

pbs_node_data(AllData, ExtractedData) ->
    [Data|RemainingData] = AllData,
    {_, _, _, _, _, _, _, _, [Datum], _, _, _} = Data,

    case Datum of
        {xmlText, [{name, _}, _, _], _, _, Hostname, text} ->
            pbs_node_data(RemainingData, [{hostname, Hostname}|ExtractedData]);

        {xmlText, [{state, _}, _, _], _, _, State, text} ->
            pbs_node_data(RemainingData, [{state, State}|ExtractedData]);

        _Else ->
            pbs_node_data(RemainingData, ExtractedData)
    end.
