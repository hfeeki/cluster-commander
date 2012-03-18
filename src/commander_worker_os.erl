%%%----------------------------------------------------------------------------
%%% @author Siraaj Khandkar <siraaj@khandkar.net>
%%%  [http://ibnfirnas.github.com]
%%% @copyright 2011-2012 Siraaj Khandkar
%%% @doc Worker backed by the operating system's "ssh"|"scp" commands.
%%%----------------------------------------------------------------------------

-module(commander_worker_os).
-export([start/2]).


-include("commander_config.hrl").
-include("commander_records.hrl").


%%%============================================================================
%%% API
%%%============================================================================

start(QueuePID, Job) ->
    % Pull-out needed options
    SaveDataTo = Job#job.save_data_to,

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
            commander_lib:do_output(Node, Output, Status, SaveDataTo),

            % Continue working
            start(QueuePID, Job);

        % No more work, so exit
        all_done -> ok
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

get_command_string(Node, Job) ->
    % Unpack options
    Operation  = Job#job.operation,
    OSCmdSSH   = Job#job.os_cmd_ssh,
    OSCmdSCP   = Job#job.os_cmd_scp,
    User       = Job#job.user,
    Port       = integer_to_list(Job#job.port),
    Timeout    = integer_to_list(trunc(Job#job.timeout)),
    Command    = Job#job.command,
    PathFrom   = Job#job.path_from,
    PathTo     = Job#job.path_to,
    MayBeQuiet = maybe_quiet(Job#job.quiet),

    % Build substrings
    UserAtNode    = User++"@"++Node,
    OptionsCommon = Port
                    ++" -o ConnectTimeout="++Timeout
                    ++" -o StrictHostKeyChecking=no"
                    ++" -o PasswordAuthentication=no"
                    ++" -2 "
                    ++MayBeQuiet,

    OptionsSCP    = "-r -P "++OptionsCommon,
    OptionsSSH    = "   -p "++OptionsCommon,

    % Assemble main string
    case Operation of
        get  ->
            From = UserAtNode++":"++PathFrom,
            To   = filename:join(PathTo, Node),
            string:join([OSCmdSCP, OptionsSCP, From, To], " ");

        put  ->
            From = PathFrom,
            To   = UserAtNode++":"++PathTo,
            string:join([OSCmdSCP, OptionsSCP, From, To], " ");

        exec ->
            string:join([OSCmdSSH, OptionsSSH, UserAtNode, Command], " ")
    end.


maybe_quiet(true)  -> "-q";
maybe_quiet(false) -> "".


do_operation_prerequisites(Node, #job{operation=get, path_to=PathTo}) ->
    filelib:ensure_dir(filename:join([PathTo, Node, "dummy_file_name"]));

do_operation_prerequisites(_Node, _Job) -> ok.
