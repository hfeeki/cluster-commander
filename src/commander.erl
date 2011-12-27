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


%%-----------------------------------------------------------------------------
%% Function : main/1
%% Purpose  : Entry point. Gets options, a list of nodes and spawns workers.
%% Type     : none()
%%-----------------------------------------------------------------------------
main(Args) ->
    %
    % Get options
    %
    Options = commander_options:get_options(Args),

    %
    % Pack job data
    %
    JobData = #job{
        user    = Options#options.user,
        command = Options#options.command,
        timeout = Options#options.host_timeout,
        port    = Options#options.port
    },

    %
    % Get a list of target nodes
    %
    MayBeTryAllNodes = Options#options.try_all_nodes,
    Nodes = commander_data:pbs_nodes(MayBeTryAllNodes),

    %
    % Start dependencies for Erlang ssh app
    %
    case Options#options.ssh_provider of
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
            Pid ! {job, Options#options.ssh_provider, JobData}
        end,
        Nodes
    ),

    %
    % Global timeout
    %
    timer:sleep(Options#options.global_timeout).
