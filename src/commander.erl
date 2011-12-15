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


%%-----------------------------------------------------------------------------
%% Function : main/0
%% Purpose  : Entry point. Gets a list of nodes and spawns worker procs.
%% Type     : none()
%%-----------------------------------------------------------------------------
main(Args) ->
    [User, Command] = [atom_to_list(Arg) || Arg <- Args],
    SshProvider = os,
    %SshProvider = otp,

    case SshProvider of
        otp ->
            crypto:start(),
            ssh:start();
        os -> pass
    end,

    Nodes = [
        Name || {{state, State}, {name, Name}} <- pbs_nodes(),
        node_available(string:tokens(State, ","))
    ],

    lists:foreach(
        fun(Node) ->
            ProcName = list_to_atom(Node),
            register(
                ProcName,
                spawn(commander, executor, [])
            ),
            ProcName ! {job, SshProvider, {User, Node, Command}}
        end,
        Nodes
    ).


%%-----------------------------------------------------------------------------
%% Function : executor/0
%% Purpose  : Executes and print output of a given SSH command.
%% Type     : none()
%%-----------------------------------------------------------------------------
executor() ->
    receive
        {job, os, {_, Host, Command}} ->
            CmdStr = string:join([?OS_SSH_CMD, Host, Command], " "),
            CmdOut = os:cmd(CmdStr),
            StdOut = string:join(["\n", Host, ?SEPARATOR, CmdOut], "\n"),
            io:format(StdOut);

        {job, otp, {User, Host, Command}} ->
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

        {ssh_cm, _, {data, _, _, Data}} ->
            {registered_name, ProcName} =
                erlang:process_info(self(), registered_name),
            NodeId = atom_to_list(ProcName),
            NodeOutput = binary_to_list(Data),
            StdOutput =
                string:join(["\n", NodeId, ?SEPARATOR, NodeOutput], "\n"),
            io:format(StdOutput);

        {ssh_cm, _, _} -> executor();

        Other ->
            io:format("WARNING! UNEXPECTED MSG: ~n~p~n", [Other]),
            executor()
    end.


%%-----------------------------------------------------------------------------
%% Function : pbs_nodes/0
%% Purpose  : Returns a list of TORQUE cluster nodes and their states.
%% Type     : list(tuple(tuple(atom(), string()), ...))
%%-----------------------------------------------------------------------------
pbs_nodes() ->
    {Tree, _} = xmerl_scan:string(os:cmd("pbsnodes -x"), [{validation, off}]),
    Nodes = nth_of_tuple(9, Tree),
    PBSNodes = [pbs_node_data(Node) || Node <- Nodes],
    PBSNodes.


%%-----------------------------------------------------------------------------
%% Function : pbs_node_data/1 -> pbs_node_data/2
%% Purpose  : Returns a tuple with a nodes name and state.
%% Type     : tuple(tuple(atom(), string()), ...)
%%-----------------------------------------------------------------------------
pbs_node_data(Node) ->
    Data = nth_of_tuple(9, Node),
    pbs_node_data(Data, []).


pbs_node_data([], ExtractedData) ->
    list_to_tuple(ExtractedData);

pbs_node_data([Data|TailData], ExtractedData) ->
    [Datum] = nth_of_tuple(9, Data),

    case Datum of
        {xmlText, [{name, _}, _, _], _, _, Name, text} ->
            pbs_node_data(TailData, [{name, Name}|ExtractedData]);

        {xmlText, [{state, _}, _, _], _, _, State, text} ->
            pbs_node_data(TailData, [{state, State}|ExtractedData]);

        _Else ->
            pbs_node_data(TailData, ExtractedData)
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


%%-----------------------------------------------------------------------------
%% Function : nth_of_tuple/2
%% Purpose  : Returns an Nth element of a tuple. Just a shortcut.
%% Type     : any()
%%-----------------------------------------------------------------------------
nth_of_tuple(N, Tuple) ->
    lists:nth(N, tuple_to_list(Tuple)).
