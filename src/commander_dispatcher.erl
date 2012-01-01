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
-export([start/1, done/1]).


%%%============================================================================
%%% API
%%%============================================================================

start(Nodes) ->
    register(dispatcher_proc,
        spawn(fun() -> dispatcher(Nodes) end)
    ).


done(Node) ->
    dispatcher_proc ! {done, Node}.


%%%============================================================================
%%% Internal
%%%============================================================================

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
