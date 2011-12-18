#Cluster Commander#


Description
=================
Takes a command string and runs it on all nodes of a TORQUE cluster that are
not marked as 'down' and/or 'offline'. Prints each output.


Caveats
=======
Assumes a password-less ssh key (current limitation in Erlang/OTP ssh app).


Roadmap
=======
1. Restructure to make a proper OTP application.
2. Use rebar and manage dependencies, releases, etc.
3. Use getopt and allow overriding defaults (set with macros).
4. Other, un-prioritized, features:
    - sftp support (get/put)
    - user-editable config files to override defaults
    - manual cluster nodes configs
    - specify individual target nodes
    - node groups
    - gated hosts (chained OS ssh/scp commands)
    - get nodes for other types of clusters (Disco, Hadoop, etc.)
    - simultaneous display of resource usage on all nodes on one screen
    - common cluster operations scripts:
        - cleanly shutdown a subset/group of nodes:
            - cleanly stop current jobs
