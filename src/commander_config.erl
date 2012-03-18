%%%----------------------------------------------------------------------------
%%% @author Siraaj Khandkar <siraaj@khandkar.net>
%%%  [http://ibnfirnas.github.com]
%%% @copyright 2011-2012 Siraaj Khandkar
%%%
%%% @doc Parsing and packaging of configuration options.
%%% @end
%%%----------------------------------------------------------------------------

-module(commander_config).
-export([get_options/1]).


-include("commander_config.hrl").
-include("commander_records.hrl").


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%%%============================================================================
%%% API
%%%============================================================================

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


%%%============================================================================
%%% Internal
%%%============================================================================

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

    WorkerModule = join_atoms([commander, worker, SSHProvider], "_"),

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
%% @doc Joins two atoms with a given separator string.
%% @spec join_atoms(ListOfAtoms::list(), Separator::string()) -> Atom
%% where
%%  Atom = atom()
%% @end
%%-----------------------------------------------------------------------------
join_atoms(Atoms, Separator) ->
    list_to_atom(string:join([atom_to_list(A) || A <- Atoms], Separator)).


%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).


join_atoms_test() ->
    test_atom = join_atoms([test, atom], "_").


-endif.
