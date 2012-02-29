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
main(Args) ->
    % Get options
    Options = get_options_or_usage(Args),

    Operation   = Options#options.operation,
    SSHProvider = Options#options.ssh_provider,

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
            do_launch(Operation, SSHProvider, Nodes, Job),

            % Wait until done or timeout
            timer:sleep(Options#options.global_timeout),
            commander_lib:commander_exit(fail, "GLOBAL TIMEOUT EXCEEDED!");

        {error, Reason} ->
            commander_lib:commander_exit(fail, Reason)
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : do_launch/4
%% Purpose  : Processes prerequisites and starts worker procs.
%% Type     : none()
%%-----------------------------------------------------------------------------
do_launch(Operation, SSHProviderRequested, Nodes, Job) ->
    Handler = commander_lib:lookup_operation_handler(Operation),

    % Temporary work-around until I implement OTP-backed transporter
    SSHProvider = case Handler of
        transporter -> os;
        executor    -> SSHProviderRequested
    end,

    HandlerModule = join_atoms([?MODULE, Handler, SSHProvider], "_"),

    do_ssh_prerequisites(SSHProvider),
    commander_dispatcher:start(Nodes),

    lists:foreach(
        fun(Node) ->
            HandlerModule:start(Node, Job, Operation)
        end,
        Nodes
    ).


do_ssh_prerequisites(os) -> none;
do_ssh_prerequisites(otp) ->
    do_ensure_ssh_key(),
    ok = crypto:start(),
    ok = ssh:start().


join_atoms(Atoms, Separator) ->
    list_to_atom(string:join([atom_to_list(A) || A <- Atoms], Separator)).


%%-----------------------------------------------------------------------------
%% Function : do_ensure_ssh_key/0 -> do_ensure_ssh_key/2
%% Purpose  : If SSH key not found, calls ssh-keygen to make one.
%% Type     : none()
%%-----------------------------------------------------------------------------
do_ensure_ssh_key() ->
    do_ensure_ssh_key(key_exists, filelib:is_file(?PATH_FILE__ID_RSA)).


do_ensure_ssh_key(key_exists, true) -> ok;
do_ensure_ssh_key(key_exists, false) ->
    do_ensure_ssh_key(key_dir, filelib:ensure_dir(?PATH_FILE__ID_RSA));

do_ensure_ssh_key(key_dir, ok) ->
    do_ensure_ssh_key(key_gen, commander_lib:os_cmd(?OS_CMD__SSH_KEYGEN));
do_ensure_ssh_key(key_dir, Error) ->
    commander_lib:commander_exit(fail, io_lib:format("~p~n", [Error]));

do_ensure_ssh_key(key_gen, {ok,   _Output}) -> ok;
do_ensure_ssh_key(key_gen, {error, Reason}) ->
    commander_lib:commander_exit(fail, Reason).


%%-----------------------------------------------------------------------------
%% Function : get_options_or_usage/1
%% Purpose  : Parses and packs CLI options and arguments into #options{} record
%% Type     : #options{} | usage()
%%-----------------------------------------------------------------------------
get_options_or_usage([]  ) -> usage();
get_options_or_usage(Args) ->
    case getopt:parse(?OPT_SPECS, Args) of
        {ok, {OptList, [OperationCandidate|Commands]=CommandsList}} ->
            Operation = list_to_atom(OperationCandidate),

            case commander_lib:lookup_operation_handler(Operation) of
                transporter when length(Commands) >= 2 ->
                    % Splitting-off tail to ignore any additional arguments:
                    [PathFrom, PathTo | _] = Commands,
                    Paths = [{from, PathFrom}, {to, PathTo}],
                    get_packed_options(OptList, Operation, [], Paths);

                transporter ->
                    usage("Please specify 2 paths: origin and destination.");

                executor ->
                    get_packed_options(OptList, Operation, Commands, []);

                unknown ->
                    Default = commander_lib:lookup_default_operation(),
                    get_packed_options(OptList, Default, CommandsList, [])
            end;

        {error, _} -> usage()
    end.


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
    commander_lib:commander_exit(fail, Message).
