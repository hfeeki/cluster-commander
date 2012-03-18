%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_records.hrl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Records definitions.
%%%----------------------------------------------------------------------------

-record(node_data,
    {
        name    = undefined :: string(),
        states  = undefined :: list(string())
    }
).

-record(job,
    {
        operation    = undefined :: atom(),
        os_cmd_ssh   = undefined :: string(),
        os_cmd_scp   = undefined :: string(),
        user         = undefined :: string(),
        command      = undefined :: string(),
        save_data_to = undefined :: string(),
        timeout      = undefined :: integer(),
        port         = undefined :: integer(),
        quiet        = undefined :: boolean(),
        path_from    = undefined :: string(),
        path_to      = undefined :: string()
    }
).

-record(nodes_opts,
    {
        nodes           = undefined :: string(),
        nodes_group     = undefined :: atom(),
        filter_pattern  = ""        :: string(),
        try_all_nodes   = undefined :: boolean()
    }
).

-record(options,
    {
        operation       = undefined :: atom(),
        path_from       = undefined :: string(),
        path_to         = undefined :: string(),
        user            = undefined :: string(),
        nodes           = undefined :: string(),
        nodes_group     = undefined :: atom(),
        ssh_provider    = undefined :: atom(),
        host_timeout    = undefined :: integer(),
        global_timeout  = undefined :: integer(),
        port            = undefined :: integer(),
        try_all_nodes   = undefined :: boolean(),
        save_data_to    = undefined :: string(),
        command         = undefined :: string()
    }
).
