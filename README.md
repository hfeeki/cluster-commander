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
```sh
$ make
$ cp ./bin/commander /some/dir/in/your/path/
```


Usage
=====
```
commander   [OPTIONS] [exec]    [COMMAND_STRING]
commander   [OPTIONS] [put|get] [FROM_PATH] [TO_PATH]

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
```

If the target command contains options itself, it must be quoted to prevent
commander from attempting to interpret those options.


Examples
--------
```sh
$ commander ls /
$ commander 'ls -la /'

# Get uptime for all hosts, with a short host timeout and no global timeout
$ commander -t 2 -T 0 -a uptime

# Disable timeouts and download /etc/hosts from all available nodes in
# group "compute_nodes_01"
$ commander -t0 -T0 -g compute_nodes_01 get /etc/hosts ./tmp
$ ls ./tmp/*/*
./tmp/node-01-01/hosts  ./tmp/node-01-03/hosts  ./tmp/node-01-05/hosts
./tmp/node-01-07/hosts  ./tmp/node-01-09/hosts
./tmp/node-01-02/hosts  ./tmp/node-01-04/hosts  ./tmp/node-01-06/hosts
./tmp/node-01-08/hosts  ./tmp/node-01-10/hosts

# Disable timeouts and push an identical /etc/hosts file to all available nodes
$ commander -t 0 -T 0 put ./tmp/etc_hosts /etc/hosts

# Attempt to run a script on all nodes, with a global deadline of 1 minute
$ commander -t0 -T60 -a /opt/collect-data.sh

# Same as above, but pass an argument to the script
$ commander -t0 -T60 -a '/opt/collect-data.sh --somearg'
```


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
