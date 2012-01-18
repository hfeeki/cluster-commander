%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_config.hrl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Global configuration knobs.
%%%----------------------------------------------------------------------------

-define(PORT, 22).
-define(TIMEOUT, 5).
-define(GLOBAL_TIMEOUT, ?TIMEOUT * 2).
-define(UNAVAILABLE_STATES, ["down", "offline"]).

-define(DEFAULT_USER, string:strip(os:cmd("whoami"), both, $\n)).
-define(DEFAULT_NODES_GROUP, pbs).

-define(PATH_DIR__DATA,     filename:join([os:getenv("HOME"), ".cluster-commander"])).
-define(PATH_DIR__DATA_SSH, filename:join([?PATH_DIR__DATA, "ssh"])).
-define(PATH_FILE__ID_RSA,  filename:join([?PATH_DIR__DATA_SSH, "id_rsa"])).
-define(PATH_FILE__GROUPS,  filename:join([?PATH_DIR__DATA, "groups.json"])).

-define(OS_CMD__SSH_KEYGEN,
    string:join(
        ["ssh-keygen", "-N", "''", "-b", "2048", "-f", ?PATH_FILE__ID_RSA],
        " "
    )
).
-define(SSH_PROVIDER, otp).  % :: otp | os
-define(CONNECT_OPTIONS,
    [
        {silently_accept_hosts, true},
        {user_interaction, true},
        {user_dir, ?PATH_DIR__DATA_SSH}
    ]
).

-define(SEPARATOR, [$- || _ <- lists:seq(1, 79)]).
-define(TERM_COLOR_EM,   "\033[0;94m").  % Blue
-define(TERM_COLOR_OK,   "\033[1;92m").  % Green bold
-define(TERM_COLOR_WARN, "\033[1;93m").  % Yellow bold
-define(TERM_COLOR_FAIL, "\033[1;91m").  % Red bold
-define(TERM_COLOR_OFF,  "\033[0m").     % COLOR OFF

-define(OPT_SPECS,
    [
        {user,           $u, "user",           {string, ?DEFAULT_USER},
       "User"
        },

        {nodes_group,   $g, "group",           {atom, ?DEFAULT_NODES_GROUP},
       "Nodes group"
        },

        {ssh_provider,   $s, "ssh",            {atom, ?SSH_PROVIDER},
       "SSH provider"
        },

        {host_timeout,   $t, "host-timeout",   {integer, ?TIMEOUT},
       "Host timeout"
        },

        {global_timeout, $T, "global-timeout", {integer, ?GLOBAL_TIMEOUT},
       "Global timeout"
        },

        {port,           $p, "port",           {integer, ?PORT},
       "SSH port number"
        },

        {try_all_nodes,   $a, "try-all-nodes", {boolean, false},
       "Attempt to connect to all nodes, regardless of their current state."
        }
    ]
).
