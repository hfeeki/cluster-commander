%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Executes a command on all available cluster nodes.
%%%----------------------------------------------------------------------------

-module(commander).
-export([main/1, dispatcher/1, executor/1]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%-----------------------------------------------------------------------------
%% Function : main/1
%% Purpose  : Entry point. Gets a list of nodes and spawns worker procs.
%% Type     : none()
%%-----------------------------------------------------------------------------
main(Args) ->
    %
    % Get options
    %
    OptSpecs = [
        {user,           $u, "user",           {string, ?DEFAULT_USER},
       "User"
        },

        {ssh_provider,   $s, "ssh",            {atom, ?SSH_PROVIDER},
       "SSH provider"
        },

        {host_timeout,   $t, "host-timeout",   {integer, ?TIMEOUT},
       "Host timeout"
        },

        {global_timeout, $T, "global-timeout", {integer, ?GLOBAL_TIMEOUT},
       "Global timeout"
        }
    ],

    {ok, OptParsed} = getopt:parse(OptSpecs, Args),
    {OptList, CommandsList} = OptParsed,

    User = proplists:get_value(user, OptList),
    SshProvider = proplists:get_value(ssh_provider, OptList),
    HostTimeout = proplists:get_value(host_timeout, OptList),
    GlobalTimeout = proplists:get_value(global_timeout, OptList),

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
        timeout = HostTimeout
    },

    %
    % Get a list of target nodes
    %
    Nodes = [
        NodeData#node_data.name || NodeData <- pbs_nodes(),
        node_available(NodeData#node_data.states)
    ],

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
    register(dispatcher_proc, spawn(fun() -> dispatcher(Nodes) end)),

    lists:foreach(
        fun(Node) ->
            Pid = spawn(fun() -> executor(Node) end),
            Pid ! {job, SshProvider, NodeJob}
        end,
        Nodes
    ),

    %
    % Global timeout
    %
    timer:sleep(GlobalTimeout * 1000).


%%-----------------------------------------------------------------------------
%% Function : stop/0
%% Purpose  : Shuts-down BEAM.
%% Type     : none()
%%-----------------------------------------------------------------------------
stop() ->
    init:stop().


%%-----------------------------------------------------------------------------
%% Function : dispatcher/1
%% Purpose  : Waits for job completion msgs and calls stop() when all are done.
%% Type     : none()
%%-----------------------------------------------------------------------------
dispatcher([]) ->
    stop();

dispatcher(Nodes) ->
    receive
        {done, Node} ->
            dispatcher(Nodes -- [Node])
    end.


%%-----------------------------------------------------------------------------
%% Function : executor/1
%% Purpose  : Executes and print output of a given SSH command.
%% Type     : none()
%%-----------------------------------------------------------------------------
executor(Node) ->
    receive
        {job, os, NodeJob} ->
            #node_job{user=User, command=Command, timeout=Timeout} = NodeJob,
            UserAtHost = string:join([User, Node], "@"),
            SshOpt =
                "-2 -q -o ConnectTimeout=" ++ integer_to_list(trunc(Timeout)),

            CmdStr = string:join(["ssh", SshOpt, UserAtHost, Command], " "),
            CmdOut = os:cmd(CmdStr),
            print(Node, CmdOut),
            self() ! done,
            executor(Node);

        {job, otp, NodeJob} ->
            #node_job{user=User, command=Command, timeout=Timeout} = NodeJob,
            TimeoutMs = Timeout * 1000,
            ConnectOptions = [
                {user, User}, {connect_timeout, TimeoutMs}
            ] ++ ?CONNECT_OPTIONS,

            case ssh:connect(Node, ?PORT, ConnectOptions) of
                {ok, ConnRef} ->
                    case ssh_connection:session_channel(ConnRef, TimeoutMs) of
                        {ok, ChannId} ->
                            ssh_connection:exec(
                                ConnRef, ChannId, Command, TimeoutMs);
                        {error, Reason} ->
                            print(Node, Reason, fail),
                            self() ! done
                    end;
                {error, Reason} ->
                    print(Node, Reason, fail),
                    self() ! done
            end,
            executor(Node);

        {ssh_cm, _, {data, _, _, Data}} ->
            print(Node, binary_to_list(Data)),
            executor(Node);

        {ssh_cm, _, {closed, _}} ->
            self() ! done,
            executor(Node);

        {ssh_cm, _} ->
            executor(Node);

        done ->
            dispatcher_proc ! {done, Node}
    end.


%%-----------------------------------------------------------------------------
%% Function : print/2 -> print/3
%% Purpose  : Labels (with Node and color code) and prints Msg to stdout.
%% Type     : none()
%%-----------------------------------------------------------------------------
print(Node, Msg) ->
    print(Node, Msg, ok).


print(Node, Msg, Flag) ->
    FormattedMsg =
        case Flag of
            fail -> io_lib:format("~p", [Msg]);
            ok   -> Msg
        end,

    MsgColor =
        case Flag of
            fail -> ?TERM_COLOR_FAIL;
            ok   -> ?TERM_COLOR_OFF
        end,

    Output = string:join(
        [
            "\n",
            string:join([?TERM_COLOR_EM, Node, ?TERM_COLOR_OFF], ""),
            string:join([?TERM_COLOR_EM, ?SEPARATOR, ?TERM_COLOR_OFF], ""),
            string:join([MsgColor, FormattedMsg, ?TERM_COLOR_OFF], "")
        ],
        "\n"
    ),

    io:format(Output).


%%-----------------------------------------------------------------------------
%% Function : pbs_nodes/0
%% Purpose  : Returns a list of TORQUE cluster nodes and their states.
%% Type     : list(#node_data{})
%%-----------------------------------------------------------------------------
pbs_nodes() ->
    {Tree, _} = xmerl_scan:string(os:cmd("pbsnodes -x"), [{validation, off}]),
    Nodes = nth_of_tuple(9, Tree),
    [pbs_node_data(Node) || Node <- Nodes].


%%-----------------------------------------------------------------------------
%% Function : pbs_node_data/1 -> pbs_node_data/2
%% Purpose  : Returns a tuple with a nodes name and state.
%% Type     : #node_data{}
%%-----------------------------------------------------------------------------
pbs_node_data(Node) ->
    Data = nth_of_tuple(9, Node),
    pbs_node_data(Data, #node_data{}).


pbs_node_data([], NodeData) -> NodeData;

pbs_node_data([Data|DataTail], NodeData) ->
    [Datum] = nth_of_tuple(9, Data),

    case Datum of
        {xmlText, [{name, _}, _, _], _, _, Name, text} ->
            pbs_node_data(DataTail, NodeData#node_data{name = Name});

        {xmlText, [{state, _}, _, _], _, _, StatesString, text} ->
            States = string:tokens(StatesString, ","),
            pbs_node_data(DataTail, NodeData#node_data{states = States});

        _Else ->
            pbs_node_data(DataTail, NodeData)
    end.


%%-----------------------------------------------------------------------------
%% Function : node_available/1
%% Purpose  : Checks whether a given node's state is considered available.
%% Type     : bool()
%%-----------------------------------------------------------------------------
node_available([]) -> true;

node_available([State|StatesTail]) ->
    case lists:member(State, ?UNAVAILABLE_STATES) of
        true -> false;
        false -> node_available(StatesTail)
    end.


%%-----------------------------------------------------------------------------
%% Function : nth_of_tuple/2
%% Purpose  : Returns an Nth element of a tuple. Just a shortcut.
%% Type     : any()
%%-----------------------------------------------------------------------------
nth_of_tuple(N, Tuple) ->
    lists:nth(N, tuple_to_list(Tuple)).
