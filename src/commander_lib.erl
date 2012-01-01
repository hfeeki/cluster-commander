%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_lib.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Shared utility functions.
%%%----------------------------------------------------------------------------

-module(commander_lib).
-export([nth_of_tuple/2]).


-include("commander_config.hrl").


%%-----------------------------------------------------------------------------
%% Function : nth_of_tuple/2
%% Purpose  : Returns an Nth element of a tuple. Just a shortcut.
%% Type     : any()
%%-----------------------------------------------------------------------------
nth_of_tuple(N, Tuple) ->
    lists:nth(N, tuple_to_list(Tuple)).
