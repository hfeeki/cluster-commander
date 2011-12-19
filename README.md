# Cluster Commander #


Description
=================
Takes a command string and runs it on all nodes of a TORQUE cluster that are
not marked as 'down' and/or 'offline'. Prints each output.


Caveats
=======
Assumes a password-less ssh key (current limitation in Erlang/OTP ssh app).


Roadmap
=======

### v1 ###
* Use getopt and allow overriding defaults (set with macros).
* Polish executable script
* Restructure to make a proper OTP application.
* Use rebar and manage dependencies, releases, etc.

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
