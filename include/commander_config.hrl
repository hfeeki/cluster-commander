%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_config.hrl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Global configuration knobs.
%%%----------------------------------------------------------------------------

-define(PORT, 22).
-define(TIMEOUT, 5000).
-define(SEPARATOR, [$- || _ <- lists:seq(1, 79)]).
-define(PATH_DIR__DATA_SSH, "../data/ssh").
