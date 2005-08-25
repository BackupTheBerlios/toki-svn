-module(toker).
-export([start/0, start/1, start_listener/1, listen/1]).

-include("toker.hrl").

start() -> toker:start(?PORT).

start(Port) ->
    register(writer, spawn(writer, start, [])),
    register(announcer, spawn(announcer, start, [])),
    register(event_handler, spawn(event_handler, start, [])),
    spawn(toker, start_listener, [Port]).

start_listener(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    toker:listen(Socket).

listen(Socket) ->
    {ok, Active_socket} = gen_tcp:accept(Socket),
    Pid = spawn(client, start, [Active_socket]),
    gen_tcp:controlling_process(Active_socket, Pid),
    toker:listen(Socket).
