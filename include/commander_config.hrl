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
-define(UNAVAILABLE_STATES, ["down", "offline"]).
-define(OS_SSH_CMD, "ssh -2 -q -o ConnectTimeout=5").
-define(SSH_PROVIDER, otp).  % :: otp | os
