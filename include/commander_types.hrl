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

-record(node_job,
    {
        user    = undefined :: string(),
        command = undefined :: string(),
        timeout = undefined :: integer(),
        port    = undefined :: integer()
    }
).
