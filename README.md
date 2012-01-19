# Cluster Commander #

Takes a command string and runs it on all nodes of a TORQUE cluster that are
not marked as 'down' and/or 'offline'. Prints each output.


Build / Install
===============
    make
    cp ./bin/commander /some/dir/in/your/path/


Usage
=====
    commander   [OPTION]... [COMMAND_STRING]

      SHORT     LONG              DESCRIPTION                  DEFAULTS TO
      -------------------------------------------------------------------------
      -u        --user            User                         <current_user>
      -g        --group           Nodes group                  pbs
      -s        --ssh             SSH provider ('os' | 'otp')  otp
      -t        --host-timeout    Host timeout                 5  <secs>
      -T        --global-timeout  Global timeout               10 <secs>
      -p        --port            SSH port number              22

      -a        --try-all-nodes   Attempt to connect to all    <OFF>
                                  nodes, regardless of their
                                  current state.

If the target command contains options itself, it must be quoted to prevent
commander from attempting to interpret those options, for example:

    commander uptime
    commander ls /
    commander "ls -l /"


Caveats
=======
Assumes a password-less ssh key (current limitation in Erlang/OTP ssh app). If
this is not acceptable, you can use '-s os' option to use system's 'ssh'
command as the alternative back-end (in which case ssh-agent must be up and
running already).


Roadmap
=======
* ~~Use rebar and manage dependencies, releases, etc.~~
* ~~Use getopt and allow overriding defaults (set with macros)~~
* ~~Polish executable script~~
* ~~node groups~~
* ~~manual cluster nodes configs~~
* specify individual target nodes on CLI
* user-editable config files to override defaults
* sftp support (get/put)
* gated hosts (chained OS ssh/scp commands)
* get nodes for other types of clusters (Disco, Hadoop, etc.)
* simultaneous display of resource usage on all nodes on one screen
* common cluster operations scripts:
    - cleanly shutdown a subset/group of nodes:
        - cleanly stop current jobs
