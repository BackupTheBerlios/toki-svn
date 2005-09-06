-module(writer).
-export([start/0, handle_files/0]).

-include("toker.hrl").

start() ->
    ?M:handle_files().

handle_files() ->
    receive
	{create_file, {Id, #event{start=S, duration=D, text=T}}} ->
	    Event = #event{start=S, duration=D, text=T},
	    write_event_file(filename(Id), Event);
	{delete_file, Id} ->
	    file:delete(filename(Id));
	{Pid, read_files} ->
	    {ok, Filenames} = file:list_dir(?DIR),
	    IDs = lists:map(fun(N) -> misc:read_integer(N) end, Filenames),
	    Sorted = lists:sort(fun(A, B) -> A > B end, IDs),
	    Events = lists:map(fun(F) -> {F, read_event_file(filename(F))} end, Sorted),
	    Pid ! lists:filter(fun({_, E}) -> E /= error end, Events);

	_Other ->
	    io:format("Writer got unexpected message: ~w~n", [_Other])
    end,
    ?M:handle_files().

filename(Id) -> lists:concat([?DIR, Id]).

write_event_file(Filename, Event) ->
    {ok, File} = file:open(Filename, write),
    write_event(File, Event),
    ok = file:close(File).

write_event(File, {event, S, D, T}) ->
    io:format(File, "start: ~w length: ~w~n~s", [S, D, T]).

read_event_file(Filename) ->
    {ok, File} = file:open(Filename, read),
    Event = read_event(File),
    file:close(File),
    Event.

read_event(File) ->
    case io:fread(File, "", "start: ~d length: ~d ") of
	{ok, [S, D]} ->
	    {ok, Text} = read_until_eof(File),
	    #event{start=S, duration=D, text=Text};
	{error, _} ->
	    error
    end.

read_until_eof(File) ->
    {ok, Cur} = file:position(File, cur),
    {ok, Eof} = file:position(File, eof),
    file:pread(File, Cur, Eof - Cur).
