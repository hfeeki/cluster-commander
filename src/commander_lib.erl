%%%----------------------------------------------------------------------------
%%% @author Siraaj Khandkar <siraaj@khandkar.net>
%%%  [http://ibnfirnas.github.com]
%%% @copyright 2011-2012 Siraaj Khandkar
%%% @doc Miscellaneous functions used throughout the project.
%%% @end
%%%----------------------------------------------------------------------------

-module(commander_lib).
-export(
    [
        commander_exit/1,
        commander_exit/2,

        do_output/4,
        do_write_data/3,
        do_print_info/2,
        do_print_data/2,
        do_print_data/3,

        lookup_exit_status/1,
        lookup_operation_type/1,
        lookup_default_operation/0,

        is_match/2,
        os_cmd/1
    ]
).


-include("commander_config.hrl").


%%%============================================================================
%%% API
%%%============================================================================

%%%----------------------------------------------------------------------------
%%% Wrappers
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% Function : do_output/4
%% Purpose  : Wrapper to print and write output.
%% Type     : io()
%%-----------------------------------------------------------------------------
do_output(Node, Data, ExitStatus, SaveDataTo) ->
    commander_lib:do_print_data(Node, Data, ExitStatus),
    commander_lib:do_write_data(Node, Data, SaveDataTo).


%%%----------------------------------------------------------------------------
%%% Major global events
%%%----------------------------------------------------------------------------

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


%%%----------------------------------------------------------------------------
%%% Writers
%%%----------------------------------------------------------------------------

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


%%%----------------------------------------------------------------------------
%%% Printers
%%%----------------------------------------------------------------------------

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


do_print_data(From, Data, ok) ->
    do_print_data(From, Data, ok, ?TERM_COLOR_OFF);

do_print_data(From, Data, fail) when is_list(Data) ->
    DataFormatted = lists:flatten(Data),
    do_print_data(From, DataFormatted, fail, ?TERM_COLOR_FAIL);

do_print_data(From, Data, fail) ->
    DataFormatted = io_lib:format("~p~n", [Data]),
    do_print_data(From, DataFormatted, fail, ?TERM_COLOR_FAIL).


do_print_data(From, Data, _Flag, Color) ->
    Output = [
        % Header
        ?TERM_COLOR_EM, From, "\n", ?SEPARATOR, ?TERM_COLOR_OFF, "\n",

        % Actual output
        Color, Data, ?TERM_COLOR_OFF, "\n"
    ],

    ok = io:format(Output).


%%%----------------------------------------------------------------------------
%%% Lookup "tables"
%%%----------------------------------------------------------------------------

lookup_exit_status(0)     -> ok;
lookup_exit_status(ok)    -> ok;
lookup_exit_status(error) -> fail;
lookup_exit_status(_)     -> fail.


lookup_operation_type(get)  -> transport;
lookup_operation_type(put)  -> transport;
lookup_operation_type(exec) -> execution;
lookup_operation_type(_)    -> unknown.


lookup_default_operation() -> exec.


%%%----------------------------------------------------------------------------
%%% Utils
%%%----------------------------------------------------------------------------

is_match(String, REPattern) ->
    re:run(String, REPattern, [{capture, none}]) =:= match.


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
