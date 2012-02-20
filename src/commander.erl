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
    % Get options
    Options = get_options_or_usage(Args),

    % Pack nodes options
    NodesOpts = #nodes_opts{
        nodes         = Options#options.nodes,
        nodes_group   = Options#options.nodes_group,
        try_all_nodes = Options#options.try_all_nodes
    },

    % Pack job options
    Job = #job{
        user         = Options#options.user,
        command      = Options#options.command,
        save_data_to = Options#options.save_data_to,
        timeout      = Options#options.host_timeout,
        port         = Options#options.port
    },

    % Get a list of target nodes
    case commander_nodes:get_nodes(NodesOpts) of
        {ok, Nodes} ->
            % Launch workers
            launch(Options#options.ssh_provider, Nodes, Job),

            % Wait until done or timeout
            timer:sleep(Options#options.global_timeout),
            commander_utils:commander_exit(fail, "GLOBAL TIMEOUT EXCEEDED!");

        {error, Reason} ->
            commander_utils:commander_exit(fail, Reason)
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : launch/3 -> launch/4
%% Purpose  : Processes prerequisites and starts worker procs.
%% Type     : none()
%%-----------------------------------------------------------------------------
launch(SshProvider, Nodes, Job) ->
    ssh_prerequisites(SshProvider),
    launch(ready, SshProvider, Nodes, Job).


launch(ready, SshProvider, Nodes, Job) ->
    Module = join_atoms([commander_executor_, SshProvider]),
    commander_dispatcher:start(Nodes),
    lists:foreach(
        fun(Node) ->
            Module:start(Node, Job)
        end,
        Nodes
    ).


ssh_prerequisites(os) -> none;
ssh_prerequisites(otp) ->
    filelib:ensure_dir(?PATH_DIR__DATA_SSH),
    file:make_dir(?PATH_DIR__DATA_SSH),
    maybe_gen_key(),
    crypto:start(),
    ssh:start().


join_atoms(ListOfAtoms) ->
    list_to_atom(string:join([atom_to_list(A) || A <- ListOfAtoms], "")).


%%-----------------------------------------------------------------------------
%% Function : maybe_gen_key/0 -> maybe_gen_key/1
%% Purpose  : If SSH key not found, calls ssh-keygen to make one.
%% Type     : none()
%%-----------------------------------------------------------------------------
maybe_gen_key() ->
    maybe_gen_key(filelib:is_file(?PATH_FILE__ID_RSA)).


maybe_gen_key(true) -> ok;
maybe_gen_key(false) -> os:cmd(?OS_CMD__SSH_KEYGEN).


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
                nodes = proplists:get_value(nodes, OptList),
                nodes_group = proplists:get_value(nodes_group, OptList),
                ssh_provider = proplists:get_value(ssh_provider, OptList),
                host_timeout = proplists:get_value(host_timeout, OptList),
                global_timeout =
                    case proplists:get_value(global_timeout, OptList) of
                        0 -> infinity;
                        OtherGlobalTimeout -> OtherGlobalTimeout * 1000
                    end,
                port = proplists:get_value(port, OptList),
                try_all_nodes = proplists:get_value(try_all_nodes, OptList),
                save_data_to = proplists:get_value(save_data_to, OptList),
                command = string:join(CommandsList, " ")
            };

        {error, _} -> usage()
    end.


%%-----------------------------------------------------------------------------
%% Function : usage/0
%% Purpose  : Prints usage instructions and exits the program.
%% Type     : none()
%%-----------------------------------------------------------------------------
usage() ->
    getopt:usage(?OPT_SPECS, ?MODULE, "command"),
    commander_utils:commander_exit(fail).
