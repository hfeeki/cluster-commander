%%%----------------------------------------------------------------------------
%%% @author Siraaj Khandkar <siraaj@khandkar.net>
%%%  [http://ibnfirnas.github.com]
%%% @copyright 2011-2012 Siraaj Khandkar
%%% @doc Entry-point module of cluster-commander.
%%%      Parses arguments and dispatches jobs.
%%% @end
%%%----------------------------------------------------------------------------

-module(commander).
-export([main/1]).


-include("commander_config.hrl").
-include("commander_types.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Entry point. Gets options, a list of nodes and spawns workers.
%%
%% @spec main(Args) -> halt(ExitStatus::integer())
%% where
%%  Args = list()
%%       | {error, Reason::term()}
%%       | {ok, Options::tuple()}
%% @end
%%-----------------------------------------------------------------------------
main(Args            ) when is_list(Args) -> main(get_options(Args));
main({error, Reason })                    -> usage(Reason);
main({ok,    {
                {job, Job},
                {nodes_opts, NodesOpts},
                {ssh_provider, SSHProvider},
                {worker_module, WorkerModule},
                {global_timeout, GlobalTimeout},
                {requested_num_workers, RequestedNumWorkers}
             }
    }) ->

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
                    % Start global timer
                    % (start_timer/3 BIF does not support 'infinity')
                    Timeout = spawn_monitor(timer, sleep, [GlobalTimeout]),

                    % Start workers
                    QueuePID   = self(),
                    NumWorkers = get_num_workers(RequestedNumWorkers,
                                                 SSHProvider,
                                                 length(Nodes)),
                    Workers  = [
                        spawn_monitor(WorkerModule, start, [QueuePID, Job])
                        || _ <- lists:seq(1, NumWorkers)
                    ],

                    % Distribute work and wait for completions
                    process_queue(Timeout, Workers, Nodes)
            end
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%%
%% @doc Distributes work to workers, waits for worker exits,
%%      then exits the program.
%% @end
%%
%% @spec process_queue(Timeout, Workers, Nodes) -> halt(ExitStatus::integer())
%% where
%%  Timeout = {pid(), reference()}
%%  Workers = list({pid(), reference()})
%%  Nodes = list(string())
%% @end
%%-----------------------------------------------------------------------------
process_queue(_Timeout, _Workers=[], _Nodes) ->
    commander_lib:commander_exit(ok);

process_queue({TimeoutPID, TimeoutRef}=Timeout, Workers, Nodes) ->
    receive
        {request_work, WorkerPID} ->
            case Nodes of
                [] ->
                    WorkerPID ! all_done,
                    process_queue(Timeout, Workers, Nodes);

                [Node|RemNodes] ->
                    WorkerPID ! {work, Node},
                    process_queue(Timeout, Workers, RemNodes)
            end;

        {'DOWN', TimeoutRef, _, TimeoutPID, normal} ->
            commander_lib:commander_exit(fail, "GLOBAL TIMEOUT EXCEEDED!");

        {'DOWN', Ref, _, PID, normal} ->
            process_queue(Timeout, lists:delete({PID, Ref}, Workers), Nodes);

        {'DOWN', Ref, _, PID, Info} ->
            commander_lib:do_print_info(fail, io_lib:format("~p~n", [Info])),
            process_queue(Timeout, lists:delete({PID, Ref}, Workers), Nodes)
    end.


%%-----------------------------------------------------------------------------
%% @doc Ensures existance of ssh keys and starts prerequisite apps.
%% @spec do_ssh_prerequisites(SSHProvider::atom()) -> ok | {error, term()}
%%-----------------------------------------------------------------------------
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

do_ssh_prerequisites(otp, ssh, ok) -> ok;
do_ssh_prerequisites(otp, ssh, Error) ->
    {error, Error}.


%%-----------------------------------------------------------------------------
%% @doc Joins two atoms with a given separator string. Shortcut
%% @spec join_atoms(ListOfAtoms:list(), Separator:string()) -> atom()
%%-----------------------------------------------------------------------------
join_atoms(Atoms, Separator) ->
    list_to_atom(string:join([atom_to_list(A) || A <- Atoms], Separator)).


%%-----------------------------------------------------------------------------
%% @doc If SSH key not found, calls ssh-keygen to make one.
%% @spec do_ensure_ssh_key() -> ok | {error, term()}
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
%% @doc Parses and packs CLI options and arguments into #options{} record
%% @spec get_options(Args::list()) -> {ok, Options} | {error, Reason}
%% where
%%  Options = tuple()
%%  Reason = string()
%% @end
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
%% @doc Packs options into records
%% @spec get_packed_options(OptList, Operation, Commands, Paths) -> {ok, Opts}
%% where
%%  OptList = list()
%%  Operation = atom()
%%  Commands = list()
%%  Paths = proplist()
%%  Opts = tuple()
%% @end
%%-----------------------------------------------------------------------------
get_packed_options(OptList, Operation, Commands, Paths) ->
    RequestedNumWorkers = proplists:get_value(workers,        OptList),
    GlobalTimeout  = case proplists:get_value(global_timeout, OptList) of
        0     -> infinity;
        Other -> Other * 1000
    end,

    % Temporary work-around until I implement OTP-backed transport (SCP)
    SSHProvider = case commander_lib:lookup_operation_type(Operation) of
        transport -> os;
        execution -> proplists:get_value(ssh_provider, OptList)
    end,

    WorkerModule = join_atoms([?MODULE, worker, SSHProvider], "_"),

    % Pack nodes options
    NodesOpts = #nodes_opts{
        nodes         = proplists:get_value(nodes,         OptList),
        nodes_group   = proplists:get_value(nodes_group,   OptList),
        try_all_nodes = proplists:get_value(try_all_nodes, OptList)
    },

    % Pack job options
    Job = #job{
        operation    = Operation,
        command      = string:join(Commands, " "),
        os_cmd_ssh   = proplists:get_value(os_cmd_ssh,   OptList),
        os_cmd_scp   = proplists:get_value(os_cmd_scp,   OptList),
        user         = proplists:get_value(user,         OptList),
        save_data_to = proplists:get_value(save_data_to, OptList),
        timeout      = proplists:get_value(host_timeout, OptList),
        port         = proplists:get_value(port,         OptList),
        quiet        = proplists:get_value(quiet,        OptList),
        path_from    = proplists:get_value(from,         Paths),
        path_to      = proplists:get_value(to,           Paths)
    },

    Options = {
        {job, Job},
        {nodes_opts, NodesOpts},
        {ssh_provider, SSHProvider},
        {worker_module, WorkerModule},
        {global_timeout, GlobalTimeout},
        {requested_num_workers, RequestedNumWorkers}
    },

    {ok, Options}.


%%-----------------------------------------------------------------------------
%% @doc How many workers should we start?
%% @spec get_num_workers(NumberRequested, SSHProvider, NumberOfNodes) -> N
%% where
%%  N = integer()
%%  NumberRequested = integer()
%%  NumberOfNodes = integer()
%%  SSHProvider = atom()
%% @end
%%-----------------------------------------------------------------------------
get_num_workers(undefined, os,  NumNodes) when NumNodes < ?DEFAULT_NUM_WORKERS
                                          -> NumNodes;
get_num_workers(undefined, os,  _)        -> ?DEFAULT_NUM_WORKERS;
get_num_workers(undefined, otp, NumNodes) -> NumNodes;
get_num_workers(Number, _, _)             -> Number.


%%-----------------------------------------------------------------------------
%% @doc Prints usage instructions and exits the program.
%% @spec usage(Message::string()) -> halt(integer())
%%-----------------------------------------------------------------------------
usage(Message) ->
    getopt:usage(?OPT_SPECS, ?MODULE, "command"),
    commander_lib:commander_exit(fail, Message).


%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).


join_atoms_test() ->
    test_atom = join_atoms([test, atom], "_").


-endif.
