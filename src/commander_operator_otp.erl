%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : coommander_operator_otp.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Operator backed by the Erlang/OTP ssh app.
%%%----------------------------------------------------------------------------

-module(commander_operator_otp).
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
%% Purpose  : Attempt SSH connection
%% Type     : loop/1 | stop/3
%%-----------------------------------------------------------------------------
init(Node, Job, _Operation) ->
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
    commander_lib:do_print_data(Node, Data, ExitStatus),
    commander_lib:do_write_data(Node, Data, SaveDataTo),
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
