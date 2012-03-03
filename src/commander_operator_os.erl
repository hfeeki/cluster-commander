%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_operator_os.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Operator backed by the operating system's "ssh"|"scp" commands.
%%%----------------------------------------------------------------------------

-module(commander_operator_os).
-export([start/3, main/3]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

start(Node, Job, Operation) ->
    spawn(?MODULE, main, [Node, Job, Operation]).


%%%============================================================================
%%% Internal
%%%============================================================================

main(Node, Job, Operation) ->
    % Ensure prerequisites
    ok = do_operation_prerequisites(Operation, Node, Job),

    % Compile command string
    OSCommandString = get_command_string(Node, Operation, Job),

    % Execute command and get output
    {ExitStatus, Output} = commander_lib:os_cmd(OSCommandString),

    % Display and save output
    Status = commander_lib:lookup_exit_status(ExitStatus),
    commander_lib:do_print_data(Node, Output, Status),
    commander_lib:do_write_data(Node, Output, Job#job.save_data_to),

    % Exit
    commander_dispatcher:done(Node).


get_command_string(Node, Operation, Job) ->
    % Unpack options
    User       = Job#job.user,
    Port       = integer_to_list(Job#job.port),
    Timeout    = integer_to_list(trunc(Job#job.timeout)),
    Command    = Job#job.command,
    PathFrom   = Job#job.path_from,
    PathTo     = Job#job.path_to,

    % Build substrings
    UserAtNode    = User++"@"++Node,
    OptionsCommon = Port++" -o ConnectTimeout="++Timeout++" -2 ",
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


do_operation_prerequisites(get, Node, Job) ->
    PathWithNodeDir = filename:join([Job#job.path_to, Node, "dummyfilename"]),
    filelib:ensure_dir(PathWithNodeDir);

do_operation_prerequisites(_Op, _Node, _Job) -> ok.
