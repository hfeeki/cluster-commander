%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_lib.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Shared utility functions.
%%%----------------------------------------------------------------------------

-module(commander_lib).
-export([nth_of_tuple/2, print/2, print/3]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%-----------------------------------------------------------------------------
%% Function : nth_of_tuple/2
%% Purpose  : Returns an Nth element of a tuple. Just a shortcut.
%% Type     : any()
%%-----------------------------------------------------------------------------
nth_of_tuple(N, Tuple) ->
    lists:nth(N, tuple_to_list(Tuple)).


%%-----------------------------------------------------------------------------
%% Function : print/2 -> print/3
%% Purpose  : Labels (with Node and color code) and prints Msg to stdout.
%% Type     : none()
%%-----------------------------------------------------------------------------
print(Node, Msg) ->
    print(Node, Msg, ok).


print(Node, Msg, Flag) ->
    FormattedMsg =
        case Flag of
            fail -> io_lib:format("~p", [Msg]);
            ok   -> Msg
        end,

    MsgColor =
        case Flag of
            fail -> ?TERM_COLOR_FAIL;
            ok   -> ?TERM_COLOR_OFF
        end,

    Output = string:join(
        [
            "\n",
            string:join([?TERM_COLOR_EM, Node, ?TERM_COLOR_OFF], ""),
            string:join([?TERM_COLOR_EM, ?SEPARATOR, ?TERM_COLOR_OFF], ""),
            string:join([MsgColor, FormattedMsg, ?TERM_COLOR_OFF], "")
        ],
        "\n"
    ),

    io:format(Output).
