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

start(Node, {job, _, JobData}) -> spawn(fun() -> executor(Node, JobData) end).


%%%============================================================================
%%% Internal
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : executor/0
%% Purpose  : Executes and prints output of a given SSH command.
%% Type     : none()
%%-----------------------------------------------------------------------------
executor(Node, JobData) ->
    UserAtHost = string:join([JobData#job.user, Node], "@"),

    SshOpt = string:join(
        [
            "-2 -q", "-p", integer_to_list(JobData#job.port),
            "-o", "ConnectTimeout="++integer_to_list(trunc(JobData#job.timeout))
        ],
        " "
    ),

    CmdStr = string:join(
        ["ssh", SshOpt, UserAtHost, JobData#job.command],
        " "
    ),

    CmdOut = os:cmd(CmdStr),

    print(Node, CmdOut).


%%-----------------------------------------------------------------------------
%% Function : print/2 -> print/3 -> print/4
%% Purpose  : Labels (with Node and color code) and prints Data to stdout.
%% Type     : none()
%%-----------------------------------------------------------------------------
print(Node, Data) ->
    print(Node, Data, ok).


print(Node, Data, ok) ->
    print(Node, Data, ok, ?TERM_COLOR_OFF);

print(Node, Data, fail) ->
    print(Node, io_lib:format("~p~n", [Data]), fail, ?TERM_COLOR_FAIL).


print(Node, Data, _Flag, Color) ->
    Output = [
        % Header
        ?TERM_COLOR_EM, Node, "\n", ?SEPARATOR, ?TERM_COLOR_OFF, "\n",

        % Actual output
        Color, Data, ?TERM_COLOR_OFF, "\n"
    ],

    io:format(Output).
