%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_dispatcher.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Dispatcher process. Waits for job completions and halts BEAM when
%%%           all are done.
%%%----------------------------------------------------------------------------

-module(commander_dispatcher).
-export([start/4]).


%%%============================================================================
%%% API
%%%============================================================================

start(Nodes, OperatorModule, Job, Operation) ->
    register(dispatcher_proc,
        spawn(fun() -> init(Nodes, OperatorModule, Job, Operation) end)
    ).


%%%============================================================================
%%% Internal
%%%============================================================================

init(Nodes, OperatorModule, Job, Operation) ->
    Workers = [OperatorModule:start(Node, Job, Operation) || Node <- Nodes],
    queue_loop(Workers).

%%-----------------------------------------------------------------------------
%% Function : queue_loop/1
%% Purpose  : Waits for job completions, then exits the program.
%% Type     : none()
%%-----------------------------------------------------------------------------
queue_loop([]) ->
    commander_lib:commander_exit(ok);

queue_loop(Workers) ->
    receive
        {'DOWN', Mref, _, Pid, normal} ->
            queue_loop(lists:delete({Pid, Mref}, Workers));

        {'DOWN', Mref, _, Pid, Info} ->
            commander_lib:do_print_info(fail, io_lib:format("~p~n", [Info])),
            queue_loop(lists:delete({Pid, Mref}, Workers))
    end.
