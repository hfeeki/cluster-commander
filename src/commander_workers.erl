%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_workers.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Worker processes.
%%%----------------------------------------------------------------------------

-module(commander_workers).
-export([dispatcher/1, executor/1]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%-----------------------------------------------------------------------------
%% Function : dispatcher/1
%% Purpose  : Waits for job completions and halts BEAM when all are done.
%% Type     : none()
%%-----------------------------------------------------------------------------
dispatcher([]) ->
    halt(0);

dispatcher(Nodes) ->
    receive
        {done, Node} ->
            dispatcher(Nodes -- [Node])
    end.


%%-----------------------------------------------------------------------------
%% Function : executor/0
%% Purpose  : Calls executor/1 with an initial (empty) data list.
%% Type     : none()
%%-----------------------------------------------------------------------------
executor(Node) ->
    executor(Node, []).


%%-----------------------------------------------------------------------------
%% Function : executor/1
%% Purpose  : Executes and print output of a given SSH command.
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
            commander_lib:print(Node, CmdOut),
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
                            commander_lib:print(Node, Reason, fail),
                            self() ! done
                    end;
                {error, Reason} ->
                    commander_lib:print(Node, Reason, fail),
                    self() ! done
            end,
            executor(Node, AccumulatedData);

        {ssh_cm, _, {data, _, _, Data}} ->
            executor(Node, [binary_to_list(Data) | AccumulatedData]);

        {ssh_cm, _, {closed, _}} ->
            JoinedData = string:join(lists:reverse(AccumulatedData), ""),
            commander_lib:print(Node, JoinedData),
            self() ! done,
            executor(Node, []);

        {ssh_cm, _} ->
            executor(Node, AccumulatedData);

        done ->
            dispatcher_proc ! {done, Node}
    end.
