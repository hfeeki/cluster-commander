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

        do_save_data/3,

        print_info/2,
        print_data/2,
        print_data/3,

        lookup_exit_status/1,
        lookup_operation_handler/1,
        lookup_default_operation/0
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
    print_info(fail, [FailTruck, Message, <<"\n">>]),
    halt(1).


%%%============================================================================
%%% Writers
%%%============================================================================

%%-----------------------------------------------------------------------------
%% Function : do_save_data/3
%% Purpose  : Save data received from node.
%% Type     : io()
%%-----------------------------------------------------------------------------
do_save_data(FromNode, Data, "") ->
    DirectoryPath = ?PATH_DIR__DATA_OUTPUTS,
    do_save_data(FromNode, Data, DirectoryPath);

do_save_data(FromNode, Data, DirectoryPath) ->
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
%% Function : print_info/2
%% Purpose  : Inform the user of something that happened in the program.
%% Type     : io()
%%-----------------------------------------------------------------------------
print_info(fail, Message) ->
    ok = io:format([?TERM_COLOR_FAIL, Message, ?TERM_COLOR_OFF, "\n"]).


%%-----------------------------------------------------------------------------
%% Function : print_data/2 -> print_data/3 -> print_data/4
%% Purpose  : Adds header and color, then prints Data to stdout.
%% Type     : none()
%%-----------------------------------------------------------------------------
print_data(From, Data) -> print_data(From, Data, ok).

print_data(From, Data, ok) -> print_data(From, Data, ok, ?TERM_COLOR_OFF);
print_data(From, Data, fail) ->
    print_data(From, io_lib:format("~p~n", [Data]), fail, ?TERM_COLOR_FAIL).

print_data(From, Data, _Flag, Color) ->
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
