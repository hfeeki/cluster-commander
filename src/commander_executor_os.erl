%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_executor_os.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Executor process (using Operating System's ssh command)
%%%----------------------------------------------------------------------------

-module(commander_executor_os).
-export([start/3, init/3]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

start(Node, Job, Operation) ->
    spawn(?MODULE, init, [Node, Job, Operation]).


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : init/3
%% Purpose  : Initializes port to system's ssh command
%% Type     : loop/1
%%-----------------------------------------------------------------------------
init(Node, Job, _Operation) ->
    % Read job options
    User       = Job#job.user,
    Port       = integer_to_list(Job#job.port),
    Timeout    = integer_to_list(trunc(Job#job.timeout)),
    Command    = Job#job.command,
    SaveDataTo = Job#job.save_data_to,

    % Compile SSH command string
    UserAtHost = string:join([User, Node], "@"),
    SSHOptions = string:join(
        ["-2", "-p", Port, "-o", "ConnectTimeout="++Timeout],
        " "
    ),
    SSHCommand = string:join(["ssh", SSHOptions, UserAtHost, Command], " "),

    % Spawn port
    PortOptions = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
    PortID = open_port({spawn, SSHCommand}, PortOptions),

    % Continue to pick-up output data
    loop(Node, PortID, SaveDataTo).


%%-----------------------------------------------------------------------------
%% Function : stop/3
%% Purpose  : Print output and exit, informing dispatcher of the completion.
%% Type     : none()
%%-----------------------------------------------------------------------------
stop(Node, Data, ExitCode, SaveDataTo) ->
    ExitStatus = commander_lib:lookup_exit_status(ExitCode),
    commander_lib:do_print_data(Node, lists:flatten(Data), ExitStatus),
    commander_lib:do_write_data(Node, Data, SaveDataTo),
    commander_dispatcher:done(Node).


%%-----------------------------------------------------------------------------
%% Function : loop/2 -> loop/3
%% Purpose  : Main loop. Collect output of the executed SSH command.
%% Type     : none()
%%-----------------------------------------------------------------------------
loop(Node, PortID, SaveDataTo) -> loop(Node, PortID, [], SaveDataTo).

loop(Node, PortID, DataAcc, SaveDataTo) ->
    receive
        {PortID, {data, Data}} ->
            loop(Node, PortID, [Data|DataAcc], SaveDataTo);

        {PortID, eof} ->
            port_close(PortID),
            receive
                {PortID, {exit_status, ExitCode}} ->
                    Data = lists:reverse(DataAcc),
                    stop(Node, Data, ExitCode, SaveDataTo)
            end
    end.
