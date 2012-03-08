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
%% Purpose  : Requests nodes from queue and executes job on them.
%% Type     : start/2 | ok
%%-----------------------------------------------------------------------------
start(QueuePID, Job) ->
    % Pull-out options
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

    % Request work
    QueuePID ! {request_work, self()},

    receive
        {work, Node} ->
            case ssh:connect(Node, Port, ConnectOptions) of
                {ok, ConnRef} ->
                    case ssh_connection:session_channel(ConnRef, Timeout) of
                        {ok, ChannId} ->
                            ssh_connection:exec(ConnRef, ChannId,
                                                Command, Timeout),

                            Data = collect_data(),
                            commander_lib:do_output(Node, Data,
                                                      ok, SaveDataTo);

                        {error, Reason} ->
                            commander_lib:do_output(Node, Reason,
                                                    fail, SaveDataTo)
                    end;
                {error, Reason} ->
                    commander_lib:do_output(Node, Reason, fail, SaveDataTo)
            end,

            % Continue working
            start(QueuePID, Job);

        % No more work, so exit
        all_done -> ok
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : collect_data/0 -> collect_data/1
%% Purpose  : Collect output of the executed SSH command.
%% Type     : list()
%%-----------------------------------------------------------------------------
collect_data() -> collect_data([]).

collect_data(DataAcc) ->
    receive
        {ssh_cm, _, {data, _, _, Data}} ->
            collect_data([Data|DataAcc]);

        {ssh_cm, _, {closed, _}} ->
            lists:reverse(DataAcc);

        {ssh_cm, _} ->
            collect_data(DataAcc)
    end.
