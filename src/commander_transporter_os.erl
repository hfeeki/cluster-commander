%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_transporter_os.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Transporter process (using Operating System's scp command)
%%%----------------------------------------------------------------------------

-module(commander_transporter_os).
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
%% Purpose  : Initializes port to system's scp command
%% Type     : loop/1
%%-----------------------------------------------------------------------------
init(Node, Job, Operation) ->
    %--------------------------------------------------------------------------
    % Read job options
    %--------------------------------------------------------------------------
    User       = Job#job.user,
    Port       = integer_to_list(Job#job.port),
    Timeout    = integer_to_list(trunc(Job#job.timeout)),
    SaveDataTo = Job#job.save_data_to,

    %--------------------------------------------------------------------------
    % Compile scp command string
    %--------------------------------------------------------------------------
    Options = string:join(
        ["-r", "-2", "-P", Port, "-o", "ConnectTimeout="++Timeout],
        " "
    ),

    {PathFrom, PathTo} =
        make_paths(Operation, User, Node, Job#job.path_from, Job#job.path_to),

    SCPCommand = string:join(["scp", Options, PathFrom, PathTo], " "),

    %--------------------------------------------------------------------------
    % Spawn port
    %--------------------------------------------------------------------------
    PortOptions = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
    PortID = open_port({spawn, SCPCommand}, PortOptions),

    %--------------------------------------------------------------------------
    % Continue to pick-up of output data
    %--------------------------------------------------------------------------
    loop(Node, PortID, SaveDataTo).


make_paths(get, User, Node, PathFrom, PathTo) ->
    ok = filelib:ensure_dir(filename:join([PathTo, Node, "dummy_file_name"])),
    From = User++"@"++Node++":"++PathFrom,
    To   = filename:join(PathTo, Node),
    {From, To};

make_paths(put, User, Node, PathFrom, PathTo) ->
    From = PathFrom,
    To   = User++"@"++Node++":"++PathTo,
    {From, To}.


%%-----------------------------------------------------------------------------
%% Function : stop/3
%% Purpose  : Print output and exit, informing dispatcher of the completion.
%% Type     : none()
%%-----------------------------------------------------------------------------
stop(Node, Data, ExitCode, SaveDataTo) ->
    ExitStatus = commander_lib:lookup_exit_status(ExitCode),
    commander_lib:do_print_data(Node, lists:flatten(Data), ExitStatus),
    commander_lib:do_save_data(Node, Data, SaveDataTo),
    commander_dispatcher:done(Node).


%%-----------------------------------------------------------------------------
%% Function : loop/2 -> loop/3
%% Purpose  : Main loop. Collect output of the executed scp command.
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
