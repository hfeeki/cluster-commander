-module(commander).
-export([main/1, executor/0]).


-define(PORT, 22).
-define(TIMEOUT, 5000).
-define(SEPARATOR, ["+" | [$- || _ <- lists:seq(1, 78)]]).
-define(PATH_DIR__DATA_SSH, "../data/ssh").


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
            io:format(
                string:join(
                    [pid_to_list(ConnRef), ?SEPARATOR, binary_to_list(Data)],
                    "\n"
                )
            ),
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