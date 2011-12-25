%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_options.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Command line arguments parsing functions.
%%%----------------------------------------------------------------------------

-module(commander_options).
-export([get_options/1]).


-include("commander_config.hrl").
-include("commander_types.hrl").


%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : get_options/1
%% Purpose  : Parses and packs CLI options and arguments into #options{} record.
%% Type     : #options{}
%%-----------------------------------------------------------------------------
get_options([]) ->
    usage();

get_options(Args) ->
    case getopt:parse(?OPT_SPECS, Args) of
        {ok, _} -> continue;
        {error, _} -> usage()
    end,

    {ok, OptParsed} = getopt:parse(?OPT_SPECS, Args),
    {OptList, CommandsList} = OptParsed,

    User = proplists:get_value(user, OptList),
    SshProvider = proplists:get_value(ssh_provider, OptList),
    HostTimeout = proplists:get_value(host_timeout, OptList),
    GlobalTimeout =
        case proplists:get_value(global_timeout, OptList) of
            0 -> infinity;
            OtherGlobalTimeout -> OtherGlobalTimeout * 1000
        end,
    Port = proplists:get_value(port, OptList),
    MayBeTryAllNodes = proplists:get_value(try_all_nodes, OptList),

    %
    % Get requested command string
    %
    Command = string:join(CommandsList, " "),

    #options{
        user = User,
        ssh_provider = SshProvider,
        host_timeout = HostTimeout,
        global_timeout = GlobalTimeout,
        port = Port,
        try_all_nodes = MayBeTryAllNodes,
        command = Command
    }.


%%%============================================================================
%%% Private
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : usage/0
%% Purpose  : Prints usage instructions and halts BEAM.
%% Type     : none()
%%-----------------------------------------------------------------------------
usage() ->
    getopt:usage(?OPT_SPECS, ?MODULE, "command"),
    halt(1).
