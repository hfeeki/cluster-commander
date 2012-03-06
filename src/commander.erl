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
main(Args            ) when is_list(Args) -> main(get_options(Args));
main({error, Reason })                    -> usage(Reason);
main({ok,    Options})                    ->
    Operation = Options#options.operation,

    % Temporary work-around until I implement OTP-backed transport (SCP)
    SSHProvider = case commander_lib:lookup_operation_type(Operation) of
        transport -> os;
        execution -> Options#options.ssh_provider
    end,

    OperatorModule = join_atoms([?MODULE, operator, SSHProvider], "_"),

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
        {error, Reason} ->
            commander_lib:commander_exit(fail, Reason);

        {ok, Nodes} ->
            case do_ssh_prerequisites(SSHProvider) of
                {error, Error} ->
                    ErrorText = io_lib:format("~p~n", [Error]),
                    commander_lib:commander_exit(fail, ErrorText);

                ok ->
                    % Dispatch workers
                    commander_dispatcher:start(Nodes, OperatorModule, Job, Operation),

                    % Wait until done or timeout
                    timer:sleep(Options#options.global_timeout),
                    commander_lib:commander_exit(fail, "GLOBAL TIMEOUT EXCEEDED!")
            end
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

do_ssh_prerequisites(os)  -> ok;
do_ssh_prerequisites(otp) ->
    do_ssh_prerequisites(otp, key, do_ensure_ssh_key()).


do_ssh_prerequisites(otp, key, ok) ->
    do_ssh_prerequisites(otp, crypto, crypto:start());
do_ssh_prerequisites(otp, key, Error) ->
    {error, Error};

do_ssh_prerequisites(otp, crypto, ok) ->
    do_ssh_prerequisites(otp, ssh, ssh:start());
do_ssh_prerequisites(otp, crypto, Error) ->
    {error, Error};

do_ssh_prerequisites(otp, ssh, ok) -> ok.


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
do_ensure_ssh_key(key_dir, Error) -> {error, Error};

do_ensure_ssh_key(key_gen, {ok,   _Output}) -> ok;
do_ensure_ssh_key(key_gen, {error, Reason}) -> {error, Reason}.


%%-----------------------------------------------------------------------------
%% Function : get_options/1
%% Purpose  : Parses and packs CLI options and arguments into #options{} record
%% Type     : {ok, #options{}} | {error, term()}
%%-----------------------------------------------------------------------------
get_options([]  ) -> {error, "Not enough arguments."};
get_options(Args) ->
    case getopt:parse(?OPT_SPECS, Args) of
        {ok, {_,       []}} ->
            {error, "Not enough arguments."};

        {ok, {OptList, [OperationCandidate|Commands]=CommandsList}} ->
            Operation = list_to_atom(OperationCandidate),

            case commander_lib:lookup_operation_type(Operation) of
                transport when length(Commands) >= 2 ->
                    % Splitting-off tail to ignore any additional arguments:
                    [PathFrom, PathTo | _] = Commands,
                    Paths = [{from, PathFrom}, {to, PathTo}],
                    get_packed_options(OptList, Operation, [], Paths);

                transport ->
                    {error, "Please specify 2 paths: origin and destination."};

                execution ->
                    get_packed_options(OptList, Operation, Commands, []);

                unknown ->
                    Default = commander_lib:lookup_default_operation(),
                    get_packed_options(OptList, Default, CommandsList, [])
            end;

        {error, Reason} ->
            {error, io_lib:format("~p~n", [Reason])}
    end.


%%-----------------------------------------------------------------------------
%% Function : get_packed_options/3
%% Purpose  : Packs options into record
%% Type     : #options{}
%%-----------------------------------------------------------------------------
get_packed_options(OptList, Operation, Commands, Paths) ->
    {ok,
        #options{
            operation      = Operation,
            command        = string:join(Commands, " "),

            path_from      = proplists:get_value(from,           Paths),
            path_to        = proplists:get_value(to,             Paths),

            user           = proplists:get_value(user,           OptList),
            nodes          = proplists:get_value(nodes,          OptList),
            nodes_group    = proplists:get_value(nodes_group,    OptList),
            ssh_provider   = proplists:get_value(ssh_provider,   OptList),
            port           = proplists:get_value(port,           OptList),
            try_all_nodes  = proplists:get_value(try_all_nodes,  OptList),
            save_data_to   = proplists:get_value(save_data_to,   OptList),
            host_timeout   = proplists:get_value(host_timeout,   OptList),
            global_timeout =
                        case proplists:get_value(global_timeout, OptList) of
                            0     -> infinity;
                            Other -> Other * 1000
                        end
        }
    }.


%%-----------------------------------------------------------------------------
%% Function : usage/1
%% Purpose  : Prints usage instructions and exits the program.
%% Type     : none()
%%-----------------------------------------------------------------------------
usage(Message) ->
    getopt:usage(?OPT_SPECS, ?MODULE, "command"),
    commander_lib:commander_exit(fail, Message).
