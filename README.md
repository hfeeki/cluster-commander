# Cluster Commander #

Takes a command string and runs it on all nodes of a TORQUE cluster that are
not marked as 'down' and/or 'offline'. Prints each output.


Build / install
===============
    make
    cp ./bin/commander /some/dir/in/your/path/


Usage
=====
    commander [-u <user>] [-s <ssh_provider>] [-t <host_timeout>]
              [-T <global_timeout>] [-p <port>] [-a <try_all_nodes>]
              command

      SHORT     LONG              DESCRIPTION                  DEAFULTS TO
      -------------------------------------------------------------------------
      -u        --user            User                         <current_user>
      -s        --ssh             SSH provider ('os' | 'otp')  otp
      -t        --host-timeout    Host timeout                 5  <secs>
      -T        --global-timeout  Global timeout               10 <secs>
      -p        --port            SSH port number              22

      -a        --try-all-nodes   Attempt to connect to all    <OFF>
                                  nodes, regardless of their
                                  current state.


Caveats
=======
Assumes a password-less ssh key (current limitation in Erlang/OTP ssh app). If
this is not acceptable, you can use '-s os' option to use system's 'ssh'
command as the alternative back-end (in which case ssh-agent must be up and
running already).


Roadmap
=======

### v1 ###
* ~~Use rebar and manage dependencies, releases, etc.~~
* ~~Use getopt and allow overriding defaults (set with macros)~~
* ~~Polish executable script~~
* Restructure to make a proper OTP application

### v2 ###
* sftp support (get/put)
* user-editable config files to override defaults
* manual cluster nodes configs
* specify individual target nodes
* node groups
* gated hosts (chained OS ssh/scp commands)
* get nodes for other types of clusters (Disco, Hadoop, etc.)
* simultaneous display of resource usage on all nodes on one screen
* common cluster operations scripts:
    - cleanly shutdown a subset/group of nodes:
        - cleanly stop current jobs
