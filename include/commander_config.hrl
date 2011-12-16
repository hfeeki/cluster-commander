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
-define(GLOBAL_TIMEOUT, ?TIMEOUT * 2).
-define(PATH_DIR__DATA_SSH, "../data/ssh").
-define(UNAVAILABLE_STATES, ["down", "offline"]).
-define(OS_SSH_CMD, "ssh -2 -q -o ConnectTimeout=5").
-define(SSH_PROVIDER, otp).  % :: otp | os
-define(CONNECT_OPTIONS,
    [
        {silently_accept_hosts, true},
        {user_interaction, true},
        {connect_timeout, ?TIMEOUT},
        {user_dir, ?PATH_DIR__DATA_SSH}
    ]
).

-define(SEPARATOR, [$- || _ <- lists:seq(1, 79)]).
-define(TERM_COLOR_EM,   "\033[0;94m").  % Blue
-define(TERM_COLOR_OK,   "\033[1;92m").  % Green bold
-define(TERM_COLOR_WARN, "\033[1;93m").  % Yellow bold
-define(TERM_COLOR_FAIL, "\033[1;91m").  % Red bold
-define(TERM_COLOR_OFF,  "\033[0m").     % COLOR OFF
