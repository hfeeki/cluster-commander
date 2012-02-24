%%%----------------------------------------------------------------------------
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

    Operation   = Options#options.operation,
    SshProvider = Options#options.ssh_provider,

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
        port         = Options#options.port,
        path_from    = Options#options.path_from,
        path_to      = Options#options.path_to
    },

    % Get a list of target nodes
    case commander_nodes:get_nodes(NodesOpts) of
        {ok, Nodes} ->
            % Launch workers
            do_launch(Operation, SshProvider, Nodes, Job),

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
%% Function : do_launch/4
%% Purpose  : Processes prerequisites and starts worker procs.
%% Type     : none()
%%-----------------------------------------------------------------------------
do_launch(exec, SshProvider, Nodes, Job) ->
    do_ssh_prerequisites(SshProvider),
    Module = join_atoms([commander_executor_, SshProvider]),
    commander_dispatcher:start(Nodes),
    lists:foreach(
        fun(Node) ->
            Module:start(Node, Job)
        end,
        Nodes
    );

do_launch(Operation, _, _, _) ->
    Msg = io_lib:format("'~s' is not yet implemented. Sorry. :(", [Operation]),
    commander_utils:commander_exit(fail, Msg).


do_ssh_prerequisites(os) -> none;
do_ssh_prerequisites(otp) ->
    filelib:ensure_dir(?PATH_DIR__DATA_SSH),
    file:make_dir(?PATH_DIR__DATA_SSH),
    do_maybe_gen_key(),
    crypto:start(),
    ssh:start().


join_atoms(ListOfAtoms) ->
    list_to_atom(string:join([atom_to_list(A) || A <- ListOfAtoms], "")).


%%-----------------------------------------------------------------------------
%% Function : do_maybe_gen_key/0 -> do_maybe_gen_key/1
%% Purpose  : If SSH key not found, calls ssh-keygen to make one.
%% Type     : none()
%%-----------------------------------------------------------------------------
do_maybe_gen_key() ->
    do_maybe_gen_key(filelib:is_file(?PATH_FILE__ID_RSA)).


do_maybe_gen_key(true) -> ok;
do_maybe_gen_key(false) -> os:cmd(?OS_CMD__SSH_KEYGEN).


%%-----------------------------------------------------------------------------
%% Function : get_options_or_usage/1
%% Purpose  : Parses and packs CLI options and arguments into #options{} record
%% Type     : #options{} | usage()
%%-----------------------------------------------------------------------------
get_options_or_usage(Args) ->
    case getopt:parse(?OPT_SPECS, Args) of
        {ok, {OptList, [OperationCandidate|Commands]=CommandsList}} ->
            Operation = list_to_atom(OperationCandidate),

            case operation_handler(Operation) of
                transporter when length(Commands) >= 2 ->
                    [PathFrom, PathTo | _] = Commands,
                    Paths = [{from, PathFrom}, {to, PathTo}],
                    get_packed_options(OptList, Operation, [], Paths);

                transporter ->
                    usage("Please specify 2 paths: origin and destination.");

                executor ->
                    get_packed_options(OptList, Operation, Commands, []);

                unknown ->
                    get_packed_options(OptList, default_operation(), CommandsList, [])
            end;

        {error, _} -> usage()
    end.


operation_handler(get)  -> transporter;
operation_handler(put)  -> transporter;
operation_handler(exec) -> executor;
operation_handler(_)    -> unknown.

default_operation() -> exec.


%%-----------------------------------------------------------------------------
%% Function : get_packed_options/3
%% Purpose  : Packs options into record
%% Type     : #options{}
%%-----------------------------------------------------------------------------
get_packed_options(OptList, Operation, Commands, Paths) ->
    #options{
        operation       = Operation,
        command         = string:join(Commands, " "),

        path_from       = proplists:get_value(from,             Paths),
        path_to         = proplists:get_value(to,               Paths),

        user            = proplists:get_value(user,             OptList),
        nodes           = proplists:get_value(nodes,            OptList),
        nodes_group     = proplists:get_value(nodes_group,      OptList),
        ssh_provider    = proplists:get_value(ssh_provider,     OptList),
        port            = proplists:get_value(port,             OptList),
        try_all_nodes   = proplists:get_value(try_all_nodes,    OptList),
        save_data_to    = proplists:get_value(save_data_to,     OptList),
        host_timeout    = proplists:get_value(host_timeout,     OptList),
        global_timeout  =
                    case  proplists:get_value(global_timeout,   OptList) of
                        0     -> infinity;
                        Other -> Other * 1000
                    end
    }.


%%-----------------------------------------------------------------------------
%% Function : usage/0 -> usage/1
%% Purpose  : Prints usage instructions and exits the program.
%% Type     : none()
%%-----------------------------------------------------------------------------
usage() -> usage("").

usage(Message) ->
    getopt:usage(?OPT_SPECS, ?MODULE, "command"),
    commander_utils:commander_exit(fail, Message).
