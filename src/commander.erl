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
-include("commander_records.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Entry point. Gets options, a list of nodes and spawns workers.
%%
%% @spec main(Args) -> halt(ExitCode)
%% where
%%  Args = list()
%%       | {error, Reason::term()}
%%       | {ok, Options::tuple()}
%%  ExitCode = integer()
%% @end
%%-----------------------------------------------------------------------------
main(Args) when is_list(Args) -> main(commander_config:get_options(Args));
main({error, Reason })        -> usage(Reason);
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
%% @spec process_queue(Timeout, Workers, Nodes) -> halt(ExitCode)
%% where
%%  Timeout = {pid(), reference()}
%%  Workers = list({pid(), reference()})
%%  Nodes = list(string())
%%  ExitCode = integer()
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
%% @spec do_ssh_prerequisites(SSHProvider::atom()) -> ok | {error, Error}
%% where
%%  Error = term()
%% @end
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
%% @doc If SSH key not found, calls ssh-keygen to make one.
%% @spec do_ensure_ssh_key() -> ok | {error, Error}
%% where
%%  Error = term()
%% @end
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
%% @spec usage(Message::string()) -> halt(ExitCode)
%% where
%%  ExitCode = integer()
%% @end
%%-----------------------------------------------------------------------------
usage(Message) ->
    getopt:usage(?OPT_SPECS, ?MODULE, "command"),
    commander_lib:commander_exit(fail, Message).


%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).
-endif.
