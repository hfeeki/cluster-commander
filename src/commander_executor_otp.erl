%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_executor_otp.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Executor process (using Erlang/OTP ssh app)
%%%----------------------------------------------------------------------------

-module(commander_executor_otp).
-export([start/2, init/2]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

start(Node, Job) ->
    spawn(?MODULE, init, [Node, Job]).


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : init/2
%% Purpose  : Attempt SSH connection
%% Type     : loop/1 | stop/3
%%-----------------------------------------------------------------------------
init(Node, Job) ->
    User       = Job#job.user,
    Port       = Job#job.port,
    Command    = Job#job.command,
    SaveDataTo = Job#job.save_data_to,
    Timeout =
        case  Job#job.timeout of
            0 -> infinity;
            OtherTimeout -> OtherTimeout * 1000
        end,

    ConnectOptions = [
        {user, User},
        {connect_timeout, Timeout}
        | ?CONNECT_OPTIONS
    ],

    case ssh:connect(Node, Port, ConnectOptions) of
        {ok, ConnRef} ->
            case ssh_connection:session_channel(ConnRef, Timeout) of
                {ok, ChannId} ->
                    ssh_connection:exec(ConnRef, ChannId, Command, Timeout),
                    loop(Node, SaveDataTo);
                {error, Reason} ->
                    stop(Node, Reason, fail, SaveDataTo)
            end;
        {error, Reason} ->
            stop(Node, Reason, fail, SaveDataTo)
    end.


%%-----------------------------------------------------------------------------
%% Function : stop/3
%% Purpose  : Print output and exit, informing dispatcher of the completion.
%% Type     : none()
%%-----------------------------------------------------------------------------
stop(Node, Data, ExitStatus, SaveDataTo) ->
    commander_utils:print(Node, Data, ExitStatus),
    commander_utils:save_data(Node, Data, SaveDataTo),
    commander_dispatcher:done(Node).


%%-----------------------------------------------------------------------------
%% Function : loop/1 -> loop/2
%% Purpose  : Main loop. Collect output of the executed SSH command.
%% Type     : none()
%%-----------------------------------------------------------------------------
loop(Node, SaveDataTo) -> loop(Node, [], SaveDataTo).

loop(Node, DataAcc, SaveDataTo) ->
    receive
        {ssh_cm, _, {data, _, _, Data}} ->
            loop(Node, [Data|DataAcc], SaveDataTo);

        {ssh_cm, _, {closed, _}} ->
            Data = lists:reverse(DataAcc),
            stop(Node, Data, ok, SaveDataTo);

        {ssh_cm, _} ->
            loop(Node, DataAcc, SaveDataTo)
    end.
