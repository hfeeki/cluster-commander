%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_executor_otp.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Executor process (using Erlang/OTP ssh app)
%%%----------------------------------------------------------------------------

-module(commander_executor_otp).
-export([start/2]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

start(Node, Job) ->
    Pid = spawn(fun() -> executor(Node) end),
    Pid ! {job, Job}.


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
executor(Node, DataAcc) ->
    receive
        {job, Job} ->
            Timeout =
                case Job#job.timeout of
                    0 -> infinity;
                    OtherTimeout -> OtherTimeout * 1000
                end,

            ConnectOptions = [
                {user, Job#job.user},
                {connect_timeout, Timeout}
                | ?CONNECT_OPTIONS
            ],

            case ssh:connect(Node, Job#job.port, ConnectOptions) of
                {ok, ConnRef} ->
                    case ssh_connection:session_channel(ConnRef, Timeout) of
                        {ok, ChannId} ->
                            ssh_connection:exec(ConnRef,
                                                ChannId,
                                                Job#job.command,
                                                Timeout);
                        {error, Reason} ->
                            commander_utils:print(Node, Reason, fail),
                            self() ! done
                    end;
                {error, Reason} ->
                    commander_utils:print(Node, Reason, fail),
                    self() ! done
            end,

            executor(Node, DataAcc);

        {ssh_cm, _, {data, _, _, Data}} ->
            executor(Node, [Data | DataAcc]);

        {ssh_cm, _, {closed, _}} ->
            Data = lists:reverse(DataAcc),
            commander_utils:print(Node, Data),
            self() ! done,
            executor(Node, []);

        {ssh_cm, _} ->
            executor(Node, DataAcc);

        done ->
            commander_dispatcher:done(Node)
    end.
