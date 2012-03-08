%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_worker_os.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Worker backed by the operating system's "ssh"|"scp" commands.
%%%----------------------------------------------------------------------------

-module(commander_worker_os).
-export([start/2]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

start(QueuePID, Job) ->
    % Request work
    QueuePID ! {request_work, self()},

    receive
        {work, Node} ->
            % Ensure prerequisites
            ok = do_operation_prerequisites(Node, Job),

            % Compile command string
            OSCommandString = get_command_string(Node, Job),

            % Execute command and get output
            {ExitStatus, Output} = commander_lib:os_cmd(OSCommandString),

            % Display and save output
            Status = commander_lib:lookup_exit_status(ExitStatus),
            do_output(Node, Output, Status, Job#job.save_data_to),

            % Continue working
            start(QueuePID, Job);

        % No more work, so exit
        all_done -> ok
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : do_output/4
%% Purpose  : Print write output.
%% Type     : io()
%%-----------------------------------------------------------------------------
do_output(Node, Data, ExitStatus, SaveDataTo) ->
    commander_lib:do_print_data(Node, Data, ExitStatus),
    commander_lib:do_write_data(Node, Data, SaveDataTo).


get_command_string(Node, Job) ->
    % Unpack options
    Operation  = Job#job.operation,
    User       = Job#job.user,
    Port       = integer_to_list(Job#job.port),
    Timeout    = integer_to_list(trunc(Job#job.timeout)),
    Command    = Job#job.command,
    PathFrom   = Job#job.path_from,
    PathTo     = Job#job.path_to,

    % Build substrings
    UserAtNode    = User++"@"++Node,
    OptionsCommon = Port
                    ++" -o ConnectTimeout="++Timeout
                    ++" -o StrictHostKeyChecking=no"
                    ++" -o PasswordAuthentication=no"
                    ++" -2 ",

    OptionsSCP    = "-r -P "++OptionsCommon,
    OptionsSSH    = "   -p "++OptionsCommon,

    % Assemble main string
    case Operation of
        get  ->
            From = UserAtNode++":"++PathFrom,
            To   = filename:join(PathTo, Node),
            string:join(["scp", OptionsSCP, From, To], " ");

        put  ->
            From = PathFrom,
            To   = UserAtNode++":"++PathTo,
            string:join(["scp", OptionsSCP, From, To], " ");

        exec ->
            string:join(["ssh", OptionsSSH, UserAtNode, Command], " ")
    end.


do_operation_prerequisites(Node, #job{operation=get, path_to=PathTo}) ->
    filelib:ensure_dir(filename:join([PathTo, Node, "dummy_file_name"]));

do_operation_prerequisites(_Node, _Job) -> ok.
