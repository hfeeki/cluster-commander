%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Executes a command on all available cluster nodes.
%%%----------------------------------------------------------------------------

-module(commander).
-export([main/1]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : main/1
%% Purpose  : Entry point. Gets options, a list of nodes and spawns workers.
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
    MayBeTryAllNodes = proplists:get_value(try_all_nodes, OptList),

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
    Nodes = commander_data:pbs_nodes(MayBeTryAllNodes),

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
    register(dispatcher_proc, spawn(commander_workers, dispatcher, [Nodes])),

    lists:foreach(
        fun(Node) ->
            Pid = spawn(commander_workers, executor, [Node]),
            Pid ! {job, SshProvider, NodeJob}
        end,
        Nodes
    ),

    %
    % Global timeout
    %
    timer:sleep(GlobalTimeout).


%%%============================================================================
%%% Private
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : usage/0
%% Purpose  : Prints usage instructions and halts BEAM.
%% Type     : none()
%%-----------------------------------------------------------------------------
usage() ->
    getopt:usage(?OPT_SPECS, ?MODULE, "command"),
    halt(1).
