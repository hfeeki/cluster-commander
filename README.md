# Cluster Commander #

Cluster management tool. Concurrently runs commands on, and
uploads/downloads files to groups of nodes.


[![Build Status](https://secure.travis-ci.org/ibnfirnas/cluster-commander.png?branch=master)](http://travis-ci.org/ibnfirnas/cluster-commander)


Usage
=====
```
commander   [OPTIONS] [exec]    [COMMAND_STRING]
commander   [OPTIONS] [put|get] [FROM_PATH] [TO_PATH]

SHORT     LONG              DESCRIPTION                    DEFAULTS TO
-------------------------------------------------------------------------
-u        --user            User                           <CURRENT_USER>
-g        --group           Nodes group                    pbs
-s        --ssh             SSH provider ('os' | 'otp')    otp
-t        --host-timeout    Host timeout                   0 (infinity)
-T        --global-timeout  Global timeout                 0 (infinity)
-p        --port            SSH port number                22
-d        --save-data-to    Directory to save outputs to.   ~/.cluster-commander/outputs
-w        --workers         Number of concurrent workers.  OS SSH: 25
                                                           OTP SSH: <NUMBER_OF_NODES>

-a        --try-all-nodes   Attempt to connect to all      <OFF>
                            nodes, regardless of their
                            current state.

-n        --nodes           Nodes list                     <EMPTY>
                            (comma-separated). Overrides
                            all other sources of nodes.
```

If the target command contains options itself, it must be quoted to prevent
commander from attempting to interpret those options.


Examples
--------
```sh
# Use quotes when remote command contains options
$ commander ls /
$ commander 'ls -la /'

# Get uptime for all hosts, with a 2-second host timeout and no global timeout
$ commander -t 2 -a uptime

# Download /etc/hosts from all available nodes in group "compute_nodes_01"
$ commander -g compute_nodes_01 get /etc/hosts ./tmp
$ ls ./tmp/*/*
./tmp/node-01-01/hosts  ./tmp/node-01-03/hosts  ./tmp/node-01-05/hosts
./tmp/node-01-07/hosts  ./tmp/node-01-09/hosts
./tmp/node-01-02/hosts  ./tmp/node-01-04/hosts  ./tmp/node-01-06/hosts
./tmp/node-01-08/hosts  ./tmp/node-01-10/hosts

# Push an identical /etc/hosts file to all available nodes
$ commander put ./tmp/etc_hosts /etc/hosts

# Attempt to run a script on all nodes, with a global deadline of 1 minute
$ commander -T60 -a /opt/collect-data.sh

# Same as above, but pass an argument to the script
$ commander -T60 -a '/opt/collect-data.sh --somearg'
```


Bootstrap
=========

Prerequisites
-------------
* Erlang R13+ (developing on R14B03, testing on R14B02-04, R15B)
* Git
* GNU Make (not REALLY needed, but simpler to build if you have it)

Download, build and install
---------------------------
```sh
$ git clone git://github.com/ibnfirnas/cluster-commander.git
$ cd cluster-commander
$ make
$ cp ./bin/commander /some/dir/in/your/path/
```

Configure nodes
---------------
By default, reads a list of nodes from `pbsnodes` command (and skips nodes
marked as 'down' and/or 'offline', overridden with `-a` CLI option).

Alternatively (or additionally), static groups of nodes can be defined in
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


Configure SSH
-------------
Do you already have password-less key-based access to all your nodes?

* YES: do the following and you're all set:

    ```sh
    $ mkdir -p ~/.cluster-commander/ssh/
    $ cp ~/.ssh/id_rsa ~/.cluster-commander/ssh/id_rsa
    ```

* NO:
    - Do you have password-protected key-based access to your nodes?
        - YES:
            - you have 3 options:
                - remove the password from the private key and
                  re-evaluate this whole, "Configure SSH" section

                - generate a new, password-less key and
                  re-evaluate this whole, "Configure SSH" section

                - start ssh-agent and use '-s os' CLI option

        - NO:
            - Generate a new key, get it on your nodes somehow and
              re-evaluate this whole, "Configure SSH", section


Note
----
If there's just no way that a password-less key is acceptable to you, you can
just always use '-s os' option to use system's 'ssh' command as the alternative
back-end (in which case ssh-agent must be up and running already).

Erlang/OTP's SSH app, currently, only supports password-less private keys.
Because Erlang's focus has been on servers and automation, not interactive use.
That said, according to Ingela Andin, there's is a solution to that in the
codebase, it just hasn't yet been ported to the SSH application (due to the
prior-mentioned priorities).

SOURCES:

* http://erlang.org/pipermail/erlang-questions/2010-April/050637.html
* https://github.com/erlang/otp/tree/master/lib/ssh/src
* https://github.com/erlang/otp/tree/master/lib/public_key/src


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
