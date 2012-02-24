%%%----------------------------------------------------------------------------
%%% Copyright (c) 2011 Siraaj Khandkar
%%% Licensed under MIT license. See LICENSE file for details.
%%%
%%% File    : commander_utils.erl
%%% Author  : Siraaj Khandkar <siraaj@khandkar.net>
%%% Purpose : Miscellaneous functions used by more than one module.
%%%----------------------------------------------------------------------------

-module(commander_utils).
-export(
    [
        commander_exit/1,
        commander_exit/2,
        save_data/3,
        print_info/2,
        print/2,
        print/3
    ]
).


-include("commander_config.hrl").


%%-----------------------------------------------------------------------------
%% Function : commander_exit/1 -> commander_exit/2
%% Purpose  : Wrapper for everything we (may) need to do to exit cleanly.
%%-----------------------------------------------------------------------------
commander_exit(ok) -> halt(0);
commander_exit(fail) -> commander_exit(fail, "").

commander_exit(fail, Message) ->
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


%%-----------------------------------------------------------------------------
%% Function : save_data/3
%% Purpose  : Save data received from node.
%% Type     : io()
%%-----------------------------------------------------------------------------
save_data(Node, Data, "") ->
    DirectoryPath = ?PATH_DIR__DATA_OUTPUTS,
    save_data(Node, Data, DirectoryPath);

save_data(Node, Data, DirectoryPath) ->
    FilePath = filename:join([DirectoryPath, Node]),
    filelib:ensure_dir(FilePath),
    file:write_file(FilePath, Data).


%%-----------------------------------------------------------------------------
%% Function : print_info/2
%% Purpose  : Inform the user of something that happened in the program.
%% Type     : io()
%%-----------------------------------------------------------------------------
print_info(fail, Message) ->
    ok = io:format([?TERM_COLOR_FAIL, Message, ?TERM_COLOR_OFF, "\n"]).


%%-----------------------------------------------------------------------------
%% Function : print/2 -> print/3 -> print/4
%% Purpose  : Adds header and color, then prints Data to stdout.
%% Type     : none()
%%-----------------------------------------------------------------------------
print(From, Data) -> print(From, Data, ok).

print(From, Data, ok) -> print(From, Data, ok, ?TERM_COLOR_OFF);
print(From, Data, fail) ->
    print(From, io_lib:format("~p~n", [Data]), fail, ?TERM_COLOR_FAIL).

print(From, Data, _Flag, Color) ->
    Output = [
        % Header
        ?TERM_COLOR_EM, From, "\n", ?SEPARATOR, ?TERM_COLOR_OFF, "\n",

        % Actual output
        Color, Data, ?TERM_COLOR_OFF, "\n"
    ],

    io:format(Output).
