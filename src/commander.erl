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
    Options = get_options_or_usage(Args),

    %
    % Pack job message
    %
    JobData = #job{
        user    = Options#options.user,
        command = Options#options.command,
        timeout = Options#options.host_timeout,
        port    = Options#options.port
    },

    JobMsg = {job, Options#options.ssh_provider, JobData},

    %
    % Get a list of target nodes
    %
    MayBeTryAllNodes = Options#options.try_all_nodes,
    Nodes = commander_nodes:pbs_nodes(MayBeTryAllNodes),

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
    % Start workerers
    %
    commander_dispatcher:start(Nodes),

    lists:foreach(
        fun(Node) ->
            commander_executor:start(Node, JobMsg)
        end,
        Nodes
    ),

    %
    % Global timeout
    %
    timer:sleep(Options#options.global_timeout).


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : get_options_or_usage/1
%% Purpose  : Parses and packs CLI options and arguments into #options{} record.
%% Type     : #options{}
%%-----------------------------------------------------------------------------
get_options_or_usage(Args) ->
    case getopt:parse(?OPT_SPECS, Args) of
        {ok, {OptList, CommandsList}} ->
            #options{
                user = proplists:get_value(user, OptList),
                ssh_provider = proplists:get_value(ssh_provider, OptList),
                host_timeout = proplists:get_value(host_timeout, OptList),
                global_timeout =
                    case proplists:get_value(global_timeout, OptList) of
                        0 -> infinity;
                        OtherGlobalTimeout -> OtherGlobalTimeout * 1000
                    end,
                port = proplists:get_value(port, OptList),
                try_all_nodes = proplists:get_value(try_all_nodes, OptList),
                command = string:join(CommandsList, " ")
            };

        {error, _} -> usage()
    end.


%%-----------------------------------------------------------------------------
%% Function : usage/0
%% Purpose  : Prints usage instructions and halts BEAM.
%% Type     : none()
%%-----------------------------------------------------------------------------
usage() ->
    getopt:usage(?OPT_SPECS, ?MODULE, "command"),
    halt(1).
