%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_types.hrl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Data types/structures definitions.
%%%----------------------------------------------------------------------------

-record(node_data,
    {
        name    = undefined :: string(),
        states  = undefined :: list(string())
    }
).

-record(job,
    {
        user    = undefined :: string(),
        command = undefined :: string(),
        timeout = undefined :: integer(),
        port    = undefined :: integer()
    }
).

-record(nodes_opts,
    {
        nodes           = undefined :: string(),
        nodes_group     = undefined :: atom(),
        try_all_nodes   = undefined :: boolean()
    }
).

-record(options,
    {
        user            = undefined :: string(),
        nodes           = undefined :: string(),
        nodes_group     = undefined :: atom(),
        ssh_provider    = undefined :: atom(),
        host_timeout    = undefined :: integer(),
        global_timeout  = undefined :: integer(),
        port            = undefined :: integer(),
        try_all_nodes   = undefined :: boolean(),
        command         = undefined :: string()
    }
).
