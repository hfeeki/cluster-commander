-module(commander).
-export([main/1, executor/0]).


-include("commander_config.hrl").


main(Args) ->
    [Host, User, Command] = lists:map(fun(A) -> atom_to_list(A) end, Args),

    crypto:start(),
    ssh:start(),

    register(executor_proc, spawn(commander, executor, [])),

    executor_proc ! {job, {Host, User, Command}}.


executor() ->
    receive
        {job, {Host, User, Command}} ->
            ConnectOptions = [
                {silently_accept_hosts, true},
                {user_interaction, true},
                {connect_timeout, ?TIMEOUT},
                {user, User},
                {user_dir, ?PATH_DIR__DATA_SSH}
            ],

            {ok, ConnRef} = ssh:connect(Host, ?PORT, ConnectOptions),
            {ok, ChannId} = ssh_connection:session_channel(ConnRef, ?TIMEOUT),

            ssh_connection:exec(ConnRef, ChannId, Command, ?TIMEOUT),
            executor();

        {ssh_cm, ConnRef, {data, _, _, Data}} ->
            NodeId = pid_to_list(ConnRef),
            NodeOutput = binary_to_list(Data),
            StdOutput = string:join([NodeId, ?SEPARATOR, NodeOutput], "\n"),

            io:format(StdOutput),

            executor_proc ! stop,
            executor();

        {ssh_cm, _, {eof, _}} -> executor();
        {ssh_cm, _, {exit_status, _, _}} -> executor();
        {ssh_cm, _, {closed, _}} -> executor();

        stop ->
            init:stop();

        Other ->
            io:format("WARNING! UNEXPECTED MSG: ~n~p~n", [Other]),
            executor()
    end.
