-module(client).
-export([start/1, handle_messages/2, action/1]).

-include("toker.hrl").

start(Socket) ->
    announcer ! {new_process, self()},
    ?M:handle_messages(Socket, queue:new()).

handle_messages(Socket, MsgQueue) ->
    receive
	disconnect ->
	    gen_tcp:close(Socket);
	{read_bytes, N, Fun} ->
	    inet:setopts(Socket, [{packet, raw}, {active, false}]),
	    {ok, Data} = gen_tcp:recv(Socket, N),
	    inet:setopts(Socket, [{packet, line}, {active, true}]),
	    ?M:handle_messages(Socket, send(Socket, MsgQueue, Fun(Data)));
	{event_handler, {_Pid, added, Id}} ->
	    ?M:handle_messages(Socket, queue:in(io_lib:format("ADDED ~B~n", [Id]), MsgQueue));
	{event_handler, {_Pid, deleted, Id}} ->
	    ?M:handle_messages(Socket, queue:in(io_lib:format("DELETED ~B~n", [Id]), MsgQueue));
	{tcp, error, closed} ->
	    io:format("Client ~w unexpectedly quit~n", [self()]);
	{tcp, Socket, Data} ->
	    ?M:handle_messages(Socket,
			       send(Socket,
				    MsgQueue,
				    client:action(string:tokens(Data, " \r\n"))));
	
	_Other ->
	    io:format("Client ~p got unexpected message: ~w~n", [self(), _Other]),
	    ?M:handle_messages(Socket, MsgQueue)
    end.

%% Come up with a better name
send(Socket, Queue, {Q, M}) ->
    if is_list(M) -> gen_tcp:send(Socket, M);
       true -> []
    end,
    case Q of
	hold_queue ->
	    Queue;
	send_queue ->
	    exhaust_queue(Queue, fun(Msg) -> gen_tcp:send(Socket, Msg) end),
	    queue:new()
    end.

exhaust_queue(Q, Fun) ->
    case queue:out(Q) of
	{{value, Item}, NewQ} ->
	    Fun(Item),
	    exhaust_queue(NewQ, Fun);
	{empty, _} -> []
    end.

%%%
%%% command(Symbol, Arglist) -> {send_queue | hold_queue, Msg | no_msg}
%%%

command(addevent, [S, D, L]) ->
    self() ! {read_bytes, L,
	      fun(Text) ->
		      event_handler ! {self(), add_event, #event{start=S, duration=D, text=Text}},
		      receive
			  {event_handler, {Pid, added, Id}} when Pid == self() ->
			      {send_queue, io_lib:format("ADDED ~B~n", [Id])}
		      end
	      end},
    {hold_queue, "HIT ME\n"};

command(getevent, [Event]) ->
    event_handler ! {self(), get_event, Event},
    receive
	{event_handler, empty} ->
	    {send_queue, "NONEXISTENT\n"};
	{event_handler, #event{start=S, duration=D, text=T}} ->
	    {send_queue, io_lib:format("~B ~B ~B~n~s", [S, D, string:len(T), T])}
    end;

command(delevent, [Event]) ->
    event_handler ! {self(), delete_event, Event},
    receive
	{event_handler, empty} ->
	    {send_queue, "NONEXISTENT\n"};
	{event_handler, {Pid, deleted, Id}} when Pid == self() ->
	    {send_queue, io_lib:format("DELETED ~B~n", [Id])}
    end;

command(numrange, [S, L]) ->
    event_handler ! {self(), range, {S, L}},
    receive
	{event_handler, []} ->
	    {send_queue, "EMPTY\n"};
	{event_handler, IDs} ->
	    {send_queue, misc:integers_to_string(IDs) ++ "\n"}
    end;

command(disconnect, []) ->
    announcer ! {self(), disconnect},
    receive
	{announcer, bye} ->
	    self() ! disconnect,
	    {hold_queue, "BYE\n"}
    end.

%%%
%%% action(Stringlist) -> {send_queue | hold_queue, Msg | no_msg}
%%%

-define(ACTION(Cmd, Word, Arglist),
	action([Word | _Args]) ->
	       case parse_arguments(_Args) of
		   Arglist -> command(Cmd, Arglist);
		   _Other  -> {send_queue, io_lib:format("ERROR: ~s: Malformed argument(s)~n", [Word])}
	       end).

?ACTION(addevent,   "ADDEVENT",   [S, D, L]);
?ACTION(getevent,   "GETEVENT",   [Event]);
?ACTION(delevent,   "DELEVENT",   [Event]);
?ACTION(numrange,   "NUMRANGE",   [S, L]);
?ACTION(disconnect, "DISCONNECT", []);

action([]) -> {send_queue, no_msg}; % received an empty line, do nothing.

action([Cmd | _]) ->
    {send_queue, io_lib:format("ERROR: Unknown command: ~s~n", [Cmd])}.

%%%
%%% parse_arguments(List) -> Integerlist
%%%

parse_arguments([]) -> [];
parse_arguments([Arg | Args]) ->
    case misc:read_integer(Arg) of
	error -> [error];
	Int   -> [Int | parse_arguments(Args)]
    end.
