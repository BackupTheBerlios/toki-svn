-module(misc).
-export([print/1, read_integer/1, integers_to_string/1, send_error/2]).

-include("toker.hrl").

-define(DEBUG, true).

print(_String) ->
    case ?DEBUG of
	true  -> io:format("~s~n", [_String]);
	false -> []
    end.

read_integer(String) ->
    case string:to_integer(String) of
	{Int, []} -> Int;
	_Other    -> error
    end.

integers_to_string([]) ->
    "";

integers_to_string([Int | []]) ->
    io_lib:format("~B", [Int]);

integers_to_string([Int | Ints]) ->
    io_lib:format("~B ", [Int]) ++ ?M:integers_to_string(Ints).

send_error(Socket, Msg) ->
    gen_tcp:send(Socket, "ERROR: " ++ Msg ++ "\n").
