-module(event_handler).
-export([start/0, handle_events/0]).

-include("toker.hrl").

start() ->
    dets:open_file(eventdb, [{file, ?DBFILE}]),
    dets:insert_new(eventdb, {0, 0}), % The ID counter
    ?M:handle_events().


handle_events() ->
    receive
	shutdown ->
	    dets:close(eventdb);

	{Pid, add_event, #event{start=S, duration=D, text=T}} ->
	    %% Increment tuple position 2 in object 0 (the counter) by 1
	    NewID = dets:update_counter(eventdb, 0, {2, 1}),
	    dets:insert(eventdb, [{NewID, #event{start=S, duration=D, text=T}}]),
	    announcer ! {broadcast, {event_handler, {Pid, added, NewID}}},
	    ?M:handle_events();

	{Pid, delete_event, Id} ->
	    case dets:member(eventdb, Id) of
		true ->
		    dets:delete(eventdb, Id),
		    announcer ! {broadcast, {event_handler, {Pid, deleted, Id}}};
		false ->
		    Pid ! {event_handler, empty}
	    end,
	    ?M:handle_events();

	{Pid, get_event, Id} when Id < 1 ->
	    Pid ! {event_handler, empty},
	    ?M:handle_events();

	{Pid, get_event, Id} ->
	    case dets:lookup(eventdb, Id) of
		[{Id, Event}] -> Pid ! {event_handler, Event};
		[]            -> Pid ! {event_handler, empty}
	    end,
	    ?M:handle_events();

	{Pid, range, {Start, Length}} ->
	    End = Start + Length,
	    %% TODO: some kind of matching would probably be better
	    IDs = dets:foldr(fun({Id, Event}, List) ->
				     if Id > 0 ->
					     case in_range(Event, Start, End) of
						 true  -> [Id | List];
						 false -> List
					     end;
					true -> List
				     end
			     end, [], eventdb),
	    Pid ! {event_handler, IDs},
	    ?M:handle_events();
	
	{Pid, disconnect} ->
	    announcer ! {disconnect, Pid},
	    ?M:handle_events();

	_Other ->
	    io:format("Event handler got unexpected message: ~w~n", [_Other]),
	    ?M:handle_events()
    end.

in_range(#event{start=Estart, duration=Elength}, Rstart, Rend) ->
    Eend = Estart + Elength,
    if Estart >= Rstart -> Estart =< Rend;
       true             -> Eend > Rstart
    end.
