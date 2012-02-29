%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_lib.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Miscellaneous functions used throughout the project.
%%%----------------------------------------------------------------------------

-module(commander_lib).
-export(
    [
        commander_exit/1,
        commander_exit/2,

        do_write_data/3,
        do_print_info/2,
        do_print_data/2,
        do_print_data/3,

        lookup_exit_status/1,
        lookup_operation_handler/1,
        lookup_default_operation/0,

        os_cmd/1
    ]
).


-include("commander_config.hrl").


%%%============================================================================
%%% Major global events
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : commander_exit/1 -> commander_exit/2
%% Purpose  : Wrapper for everything we (may) need to do to exit cleanly.
%%-----------------------------------------------------------------------------
commander_exit(ok) -> halt(0);
commander_exit(fail) -> commander_exit(fail, "").

commander_exit(fail, Message) ->
    %
    % Fail Truck courtesy of Al Barrentine (https://github.com/thatdatabaseguy)
    %
    FailTruck =
        <<"
        UH-OH! YOU'VE BEEN HIT BY THE

        |^^^^^^^^^^^^^^^^|
        |   FAIL TRUCK   | Ô|ÓÓ;.., ___.
        |_É_É______========|= _|__|É, ] |
        (@ )Õ(@ )ÓÓÓÓ*|(@ )(@ )******(@)

        ">>,
    do_print_info(fail, [FailTruck, Message, <<"\n">>]),
    halt(1).


%%%============================================================================
%%% Writers
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : do_write_data/3
%% Purpose  : Save data received from node.
%% Type     : io()
%%-----------------------------------------------------------------------------
do_write_data(FromNode, Data, "") ->
    DirectoryPath = ?PATH_DIR__DATA_OUTPUTS,
    do_write_data(FromNode, Data, DirectoryPath);

do_write_data(FromNode, Data, DirectoryPath) ->
    FilePath = filename:join([DirectoryPath, FromNode]),
    FormattedData = case (is_binary(Data) or is_list(Data)) of
        true  -> Data;
        false -> io_lib:format("~p~n", [Data])
    end,
    ok = filelib:ensure_dir(FilePath),
    ok = file:write_file(FilePath, FormattedData).


%%%============================================================================
%%% Printers
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : do_print_info/2
%% Purpose  : Inform the user of something that happened in the program.
%% Type     : io()
%%-----------------------------------------------------------------------------
do_print_info(fail, Message) ->
    ok = io:format([?TERM_COLOR_FAIL, Message, ?TERM_COLOR_OFF, "\n"]).


%%-----------------------------------------------------------------------------
%% Function : do_print_data/2 -> do_print_data/3 -> do_print_data/4
%% Purpose  : Adds header and color, then prints Data to stdout.
%% Type     : none()
%%-----------------------------------------------------------------------------
do_print_data(From, Data) -> do_print_data(From, Data, ok).

do_print_data(From, Data, ok) -> do_print_data(From, Data, ok, ?TERM_COLOR_OFF);
do_print_data(From, Data, fail) ->
    do_print_data(From, io_lib:format("~p~n", [Data]), fail, ?TERM_COLOR_FAIL).

do_print_data(From, Data, _Flag, Color) ->
    Output = [
        % Header
        ?TERM_COLOR_EM, From, "\n", ?SEPARATOR, ?TERM_COLOR_OFF, "\n",

        % Actual output
        Color, Data, ?TERM_COLOR_OFF, "\n"
    ],

    ok = io:format(Output).


%%%============================================================================
%%% Lookup "tables"
%%%============================================================================

lookup_exit_status(0) -> ok;
lookup_exit_status(_) -> fail.


lookup_operation_handler(get)  -> transporter;
lookup_operation_handler(put)  -> transporter;
lookup_operation_handler(exec) -> executor;
lookup_operation_handler(_)    -> unknown.


lookup_default_operation() -> exec.


%%%============================================================================
%%% Utils
%%%============================================================================

os_cmd(Command) ->
    PortOptions = [stream, exit_status, use_stdio, stderr_to_stdout, in, eof],
    PortID = open_port({spawn, Command}, PortOptions),
    {ExitCode, Output} = pickup_output(PortID, []),

    case lookup_exit_status(ExitCode) of
        fail -> {error, Output};
        ok   -> {ok,    Output}
    end.


%%%============================================================================
%%% Internal
%%%============================================================================

pickup_output(PortID, DataAcc) ->
    receive
        {PortID, {data, Data}} ->
            pickup_output(PortID, [Data|DataAcc]);

        {PortID, eof} ->
            port_close(PortID),
            receive
                {PortID, {exit_status, ExitCode}} ->
                    Output = lists:flatten(lists:reverse(DataAcc)),
                    {ExitCode, Output}
            end
    end.
