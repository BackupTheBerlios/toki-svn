-module(event_handler).
-export([start/0, handle_events/2]).

-include("toker.hrl").

start() ->
    writer ! {self(), read_files},
    receive
	[{Id, E} | Es] ->
	    ?M:handle_events([{Id, E} | Es], Id + 1);
	[] ->
	    ?M:handle_events([], 1)
    end.

handle_events(Events, NextID) ->
    receive
	synchronize ->
	    %% first, ask writer to read all files and return events in a list.
	    %% if...:
	    %% - ID in both lists:
	    %%	    replace event in memory with event from file
	    %% - ID in file but not in memory:
	    %%	    add event to memory
	    %% - ID in memory but not in file:
	    %%	    create event file
	    [];

	{Pid, add_event, #event{start=S, duration=D, text=T}} ->
	    Pair = {NextID, #event{start=S, duration=D, text=T}},
	    writer ! {create_file, Pair},
	    announcer ! {broadcast, {event_handler, {Pid, added, NextID}}},
	    ?M:handle_events([Pair | Events], NextID + 1);

	{Pid, delete_event, Id} ->
	    case lists:keymember(Id, 1, Events) of
		true ->
		    writer ! {delete_file, Id},
		    announcer ! {broadcast, {event_handler, {Pid, deleted, Id}}},
		    ?M:handle_events(lists:keydelete(Id, 1, Events), NextID);
		false ->
		    Pid ! {event_handler, empty},
		    ?M:handle_events(Events, NextID)
	    end;

	{Pid, get_event, Id} ->
	    Pid ! case lists:keysearch(Id, 1, Events) of
		      {value, {_, Event}} -> {event_handler, Event};
		      false               -> {event_handler, empty}
		  end,
	    ?M:handle_events(Events, NextID);

	{Pid, range, {Start, Length}} ->
	    End = Start + Length,
	    Range = lists:filter(fun({_, #event{start=S}}) -> Start =< S andalso S =< End end, Events),
	    Pid ! {event_handler, lists:map(fun({Id, _}) -> Id end, Range)},
	    ?M:handle_events(Events, NextID);

	_Other ->
	    io:format("Event handler got unexpected message: ~w~n", [_Other]),
	    ?M:handle_events(Events, NextID)
    end.
