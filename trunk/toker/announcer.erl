-module(announcer).
-export([start/0, handle_procs/1]).

-include("toker.hrl").

start() ->
    ?M:handle_procs([]).

handle_procs(Procs) ->
    receive
	{new_process, Pid} ->
	    ?M:handle_procs([Pid | Procs]);
	{disconnect, Pid} ->
	    ?M:handle_procs(lists:delete(Pid, Procs));
	{broadcast, Message} ->
	    lists:foreach(fun(P) -> P ! Message end, Procs),
	    ?M:handle_procs(Procs);

	_Other ->
	    io:format("Process handler got unexpected message: ~w~n", [_Other]),
	    ?M:handle_procs(Procs)
    end.
