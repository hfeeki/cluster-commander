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
    %--------------------------------------------------------------------------
    % Read job options
    %--------------------------------------------------------------------------
    User       = Job#job.user,
    Port       = integer_to_list(Job#job.port),
    Timeout    = integer_to_list(trunc(Job#job.timeout)),
    Command    = Job#job.command,
    SaveDataTo = Job#job.save_data_to,

    %--------------------------------------------------------------------------
    % Compile SSH command string
    %--------------------------------------------------------------------------
    UserAtHost = string:join([User, Node], "@"),
    SSHOptions = string:join(
        ["-2", "-p", Port, "-o", "ConnectTimeout="++Timeout],
        " "
    ),
    SSHCommand = string:join(["ssh", SSHOptions, UserAtHost, Command], " "),

    %--------------------------------------------------------------------------
    % Execute command and get output
    %--------------------------------------------------------------------------
    {ExitStatus, Output} = commander_lib:os_cmd(SSHCommand),

    %--------------------------------------------------------------------------
    % Display and save output
    %--------------------------------------------------------------------------
    Status = commander_lib:lookup_exit_status(ExitStatus),
    commander_lib:do_print_data(Node, Output, Status),
    commander_lib:do_write_data(Node, Output, SaveDataTo),

    %--------------------------------------------------------------------------
    % Exit
    %--------------------------------------------------------------------------
    commander_dispatcher:done(Node).
