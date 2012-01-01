%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_executor.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Executes and prints output of a given SSH command.
%%%----------------------------------------------------------------------------

-module(commander_executor).
-export([start/2]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

start(Node, JobMsg) ->
    Pid = spawn(fun() -> executor(Node) end),
    Pid ! JobMsg.


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : executor/0
%% Purpose  : Calls executor/1 with an initial (empty) data list.
%% Type     : none()
%%-----------------------------------------------------------------------------
executor(Node) ->
    executor(Node, []).


%%-----------------------------------------------------------------------------
%% Function : executor/1
%% Purpose  : Executes and prints output of a given SSH command.
%% Type     : none()
%%-----------------------------------------------------------------------------
executor(Node, AccumulatedData) ->
    receive
        {job, os, JobData} ->
            UserAtHost = string:join([JobData#job.user, Node], "@"),
            SshOpt = string:join([
                "-2 -q",
                "-p",
                integer_to_list(JobData#job.port),
                "-o",
                "ConnectTimeout="++integer_to_list(trunc(JobData#job.timeout))
            ], " "),

            CmdStr = string:join(
                ["ssh", SshOpt, UserAtHost, JobData#job.command],
                " "
            ),

            CmdOut = os:cmd(CmdStr),
            print(Node, CmdOut),
            self() ! done,
            executor(Node, AccumulatedData);

        {job, otp, JobData} ->
            Timeout =
                case JobData#job.timeout of
                    0 -> infinity;
                    OtherTimeout -> OtherTimeout * 1000
                end,

            ConnectOptions =
                [
                    {user, JobData#job.user},
                    {connect_timeout, Timeout}
                    | ?CONNECT_OPTIONS
                ],

            case ssh:connect(Node, JobData#job.port, ConnectOptions) of
                {ok, ConnRef} ->
                    case ssh_connection:session_channel(ConnRef, Timeout) of
                        {ok, ChannId} ->
                            ssh_connection:exec(ConnRef,
                                                ChannId,
                                                JobData#job.command,
                                                Timeout);
                        {error, Reason} ->
                            print(Node, Reason, fail),
                            self() ! done
                    end;
                {error, Reason} ->
                    print(Node, Reason, fail),
                    self() ! done
            end,
            executor(Node, AccumulatedData);

        {ssh_cm, _, {data, _, _, Data}} ->
            executor(Node, [binary_to_list(Data) | AccumulatedData]);

        {ssh_cm, _, {closed, _}} ->
            JoinedData = string:join(lists:reverse(AccumulatedData), ""),
            print(Node, JoinedData),
            self() ! done,
            executor(Node, []);

        {ssh_cm, _} ->
            executor(Node, AccumulatedData);

        done ->
            commander_dispatcher:done(Node)
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