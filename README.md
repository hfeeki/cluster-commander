# Cluster Commander #

Cluster management tool. Concurrently runs command(s) on groups of nodes.

By default, reads a list of nodes from `pbsnodes` command (and skips nodes
marked as 'down' and/or 'offline', overridden with `-a` CLI option).
Alternatively (or additionaly), groups of nodes can be defined in
`~/.cluster-commander/groups.json`, for example:

```json
{
    "file_servers": [
        "fs-01",
        "fs-02",
        "fs-03"
    ],
    "compute_nodes_01": [
        "node-01-01",
        "node-01-02",
        "node-01-03"
    ],
    "compute_nodes_02": [
        "node-02-01",
        "node-02-02",
        "node-02-03"
    ]
}
```


Build / Install
===============
    make
    cp ./bin/commander /some/dir/in/your/path/


Usage
=====
    commander   [OPTION]... [COMMAND_STRING]

      SHORT     LONG              DESCRIPTION                  DEFAULTS TO
      -------------------------------------------------------------------------
      -u        --user            User                         <CURRENT_USER>
      -g        --group           Nodes group                  pbs
      -s        --ssh             SSH provider ('os' | 'otp')  otp
      -t        --host-timeout    Host timeout                 5  <SECS>
      -T        --global-timeout  Global timeout               10 <SECS>
      -p        --port            SSH port number              22

      -a        --try-all-nodes   Attempt to connect to all    <OFF>
                                  nodes, regardless of their
                                  current state.

      -n        --nodes           Nodes list                   <EMPTY>
                                  (comma-separated). Overrides
                                  all other sources of nodes.

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
* ~~specify individual target nodes on CLI~~
* Accept a (whitespace-delimited) list of nodes from stdin
* Organize node-groups configurations into "static" and "dynamic" groups:
    - Static groups: defined manually, on a per-node basis
        - Add helper commands to generate and update static groups based
          on defined naming patterns, such as: node-01-01, node-01-02,
          node-02-01, ..., etc

    - Built-in dynamic groups: built-in parsers for common cluster resource
      managers (with options for node state handling ("down", "offline", etc)):
        - ~~TORQUE/PBS~~
        - Disco
        - Hadoop

    - Plug-in dynamic groups: define an external command whose output supplies
      a whitespace-delimited list of nodes

* CLI option to filter nodes through a RegEx pattern
* user-editable config files to override defaults
* ~~sftp support (get/put) via 'scp' command~~
* sftp support (get/put) via OTP ssh app
* gated hosts:
    - chained OS ssh/scp commands?
    - setup tunnels?
* Persistent, interactive sessions
* common cluster operations scripts:
    - cleanly shutdown a subset/group of nodes:
        - cleanly stop current jobs
* Tests...
* Test a node's availability based on latency:
    - ICMP
    - SSH
* Configurable latency threshold parameter(s) to determine whether we consider
  a node "available"
* Organize how generation of missing keys is handled.
