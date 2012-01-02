%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_executor.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Executor process (using Operating System's ssh command)
%%%----------------------------------------------------------------------------

-module(commander_executor_os).
-export([start/2]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

start(Node, Job) -> spawn(fun() -> executor(Node, Job) end).


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : executor/0
%% Purpose  : Executes and prints output of a given SSH command.
%% Type     : none()
%%-----------------------------------------------------------------------------
executor(Node, Job) ->
    UserAtHost = string:join([Job#job.user, Node], "@"),

    SshOpt = string:join(
        [
            "-2 -q", "-p", integer_to_list(Job#job.port),
            "-o", "ConnectTimeout="++integer_to_list(trunc(Job#job.timeout))
        ],
        " "
    ),

    CmdStr = string:join(
        ["ssh", SshOpt, UserAtHost, Job#job.command],
        " "
    ),

    CmdOut = os:cmd(CmdStr),

    commander_utils:print(Node, CmdOut).
