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
        name   = "" :: string(),
        states = [] :: list(string())
    }
).
