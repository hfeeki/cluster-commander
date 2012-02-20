%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_utils.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Miscellaneous functions used by more than one module.
%%%----------------------------------------------------------------------------

-module(commander_utils).
-export(
    [
        commander_exit/1,
        commander_exit/2,
        print_info/2,
        print/2,
        print/3
    ]
).


-include("commander_config.hrl").


%%-----------------------------------------------------------------------------
%% Function : commander_exit/1 -> commander_exit/2
%% Purpose  : Wrapper for everything we (may) need to do to exit cleanly.
%%-----------------------------------------------------------------------------
commander_exit(ok) -> halt(0);
commander_exit(fail) -> commander_exit(fail, "").

commander_exit(fail, Message) ->
    print_info(fail, Message),
    halt(1).


%%-----------------------------------------------------------------------------
%% Function : print_info/2
%% Purpose  : Inform the user of something that happened in the program.
%% Type     : io()
%%-----------------------------------------------------------------------------
print_info(fail, Message) ->
    ok = io:format([?TERM_COLOR_FAIL, Message, ?TERM_COLOR_OFF, "\n"]).


%%-----------------------------------------------------------------------------
%% Function : print/2 -> print/3 -> print/4
%% Purpose  : Adds header and color, then prints Data to stdout.
%% Type     : none()
%%-----------------------------------------------------------------------------
print(From, Data) -> print(From, Data, ok).

print(From, Data, ok) -> print(From, Data, ok, ?TERM_COLOR_OFF);
print(From, Data, fail) ->
    print(From, io_lib:format("~p~n", [Data]), fail, ?TERM_COLOR_FAIL).

print(From, Data, _Flag, Color) ->
    Output = [
        % Header
        ?TERM_COLOR_EM, From, "\n", ?SEPARATOR, ?TERM_COLOR_OFF, "\n",

        % Actual output
        Color, Data, ?TERM_COLOR_OFF, "\n"
    ],

    io:format(Output).
