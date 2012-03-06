%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : coommander_worker_otp.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Worker backed by the Erlang/OTP ssh app.
%%%----------------------------------------------------------------------------

-module(commander_worker_otp).
-export([start/2]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : start/2
%% Purpose  : Attempt SSH connection
%% Type     : collect_data/1 | stop/4
%%-----------------------------------------------------------------------------
start(Node, Job) ->
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
                    collect_data(Node, SaveDataTo);
                {error, Reason} ->
                    stop(Node, Reason, fail, SaveDataTo)
            end;
        {error, Reason} ->
            stop(Node, Reason, fail, SaveDataTo)
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : stop/4
%% Purpose  : Print output and exit, informing dispatcher of the completion.
%% Type     : none()
%%-----------------------------------------------------------------------------
stop(Node, Data, ExitStatus, SaveDataTo) ->
    commander_lib:do_print_data(Node, Data, ExitStatus),
    commander_lib:do_write_data(Node, Data, SaveDataTo).


%%-----------------------------------------------------------------------------
%% Function : collect_data/2 -> collect_data/3
%% Purpose  : Main loop. Collect output of the executed SSH command.
%% Type     : none()
%%-----------------------------------------------------------------------------
collect_data(Node, SaveDataTo) -> collect_data(Node, [], SaveDataTo).

collect_data(Node, DataAcc, SaveDataTo) ->
    receive
        {ssh_cm, _, {data, _, _, Data}} ->
            collect_data(Node, [Data|DataAcc], SaveDataTo);

        {ssh_cm, _, {closed, _}} ->
            Data = lists:reverse(DataAcc),
            stop(Node, Data, ok, SaveDataTo);

        {ssh_cm, _} ->
            collect_data(Node, DataAcc, SaveDataTo)
    end.
