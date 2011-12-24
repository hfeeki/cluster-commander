%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Executes a command on all available cluster nodes.
%%%----------------------------------------------------------------------------

-module(commander).
-export([main/1, dispatcher/1, executor/1]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%-----------------------------------------------------------------------------
%% Function : main/1
%% Purpose  : Entry point. Gets a list of nodes and spawns worker procs.
%% Type     : none()
%%-----------------------------------------------------------------------------
main([]) ->
    usage();

main(Args) ->
    %
    % Get options
    %
    case getopt:parse(?OPT_SPECS, Args) of
        {ok, _} -> continue;
        {error, _} -> usage()
    end,

    {ok, OptParsed} = getopt:parse(?OPT_SPECS, Args),
    {OptList, CommandsList} = OptParsed,

    User = proplists:get_value(user, OptList),
    SshProvider = proplists:get_value(ssh_provider, OptList),
    HostTimeout = proplists:get_value(host_timeout, OptList),
    GlobalTimeout =
        case proplists:get_value(global_timeout, OptList) of
            0 -> infinity;
            OtherGlobalTimeout -> OtherGlobalTimeout * 1000
        end,
    Port = proplists:get_value(port, OptList),
    TryAllNodes = proplists:get_value(try_all_nodes, OptList),

    %
    % Get requested command string
    %
    Command = string:join(CommandsList, " "),

    %
    % Pack job
    %
    NodeJob = #node_job{
        user    = User,
        command = Command,
        timeout = HostTimeout,
        port = Port
    },

    %
    % Get a list of target nodes
    %
    Nodes =
    [
        NodeData#node_data.name || NodeData <- commander_data:pbs_nodes(),
        case TryAllNodes of
            true -> true;
            false -> commander_data:node_available(NodeData#node_data.states)
        end
    ],

    %
    % Start dependencies for Erlang ssh app
    %
    case SshProvider of
        otp ->
            filelib:ensure_dir(?PATH_DIR__DATA_SSH),
            file:make_dir(?PATH_DIR__DATA_SSH),

            case filelib:is_file(?PATH_FILE__ID_RSA) of
                true -> pass;
                false -> os:cmd(?OS_CMD__SSH_KEYGEN)
            end,

            crypto:start(),
            ssh:start();
        os -> pass
    end,

    %
    % Start worker procs
    %
    register(dispatcher_proc, spawn(commander, dispatcher, [Nodes])),

    lists:foreach(
        fun(Node) ->
            Pid = spawn(commander, executor, [Node]),
            Pid ! {job, SshProvider, NodeJob}
        end,
        Nodes
    ),

    %
    % Global timeout
    %
    timer:sleep(GlobalTimeout).


%%-----------------------------------------------------------------------------
%% Function : usage/0
%% Purpose  : Prints usage instructions and halts BEAM.
%% Type     : none()
%%-----------------------------------------------------------------------------
usage() ->
    getopt:usage(?OPT_SPECS, ?MODULE, "command"),
    halt(1).


%%-----------------------------------------------------------------------------
%% Function : dispatcher/1
%% Purpose  : Waits for job completions and halts BEAM when all are done.
%% Type     : none()
%%-----------------------------------------------------------------------------
dispatcher([]) ->
    halt(0);

dispatcher(Nodes) ->
    receive
        {done, Node} ->
            dispatcher(Nodes -- [Node])
    end.


%%-----------------------------------------------------------------------------
%% Function : executor/0
%% Purpose  : Calls executor/1 with an initial (empty) data list.
%% Type     : none()
%%-----------------------------------------------------------------------------
executor(Node) ->
    executor(Node, []).


%%-----------------------------------------------------------------------------
%% Function : executor/1
%% Purpose  : Executes and print output of a given SSH command.
%% Type     : none()
%%-----------------------------------------------------------------------------
executor(Node, AccumulatedData) ->
    receive
        {job, os, NodeJob} ->
            #node_job{
                user=User, command=Command, timeout=Timeout, port=Port
            } = NodeJob,

            UserAtHost = string:join([User, Node], "@"),
            SshOpt = string:join([
                "-2 -q",

                "-p",
                integer_to_list(Port),

                "-o",
                "ConnectTimeout=" ++ integer_to_list(trunc(Timeout))
            ], " "),

            CmdStr = string:join(["ssh", SshOpt, UserAtHost, Command], " "),
            CmdOut = os:cmd(CmdStr),
            commander_lib:print(Node, CmdOut),
            self() ! done,
            executor(Node, AccumulatedData);

        {job, otp, NodeJob} ->
            #node_job{
                user=User, command=Command, timeout=Timeout, port=Port
            } = NodeJob,

            TimeoutMs =
                case Timeout of
                    0 -> infinity;
                    OtherTimeout -> OtherTimeout * 1000
                end,

            ConnectOptions =
                [{user, User}, {connect_timeout, TimeoutMs}|?CONNECT_OPTIONS],

            case ssh:connect(Node, Port, ConnectOptions) of
                {ok, ConnRef} ->
                    case ssh_connection:session_channel(ConnRef, TimeoutMs) of
                        {ok, ChannId} ->
                            ssh_connection:exec(
                                ConnRef, ChannId, Command, TimeoutMs);
                        {error, Reason} ->
                            commander_lib:print(Node, Reason, fail),
                            self() ! done
                    end;
                {error, Reason} ->
                    commander_lib:print(Node, Reason, fail),
                    self() ! done
            end,
            executor(Node, AccumulatedData);

        {ssh_cm, _, {data, _, _, Data}} ->
            executor(Node, [binary_to_list(Data) | AccumulatedData]);

        {ssh_cm, _, {closed, _}} ->
            JoinedData = string:join(lists:reverse(AccumulatedData), ""),
            commander_lib:print(Node, JoinedData),
            self() ! done,
            executor(Node, []);

        {ssh_cm, _} ->
            executor(Node, AccumulatedData);

        done ->
            dispatcher_proc ! {done, Node}
    end.
