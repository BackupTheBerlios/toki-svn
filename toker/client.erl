-module(client).
-export([start/1, handle_messages/2, handle_socket/2]).

-include("toker.hrl").

start(Socket) ->
    Below = spawn(?M, handle_socket, [Socket, self()]),
    gen_tcp:controlling_process(Socket, Below),
    ?M:handle_messages(queue:new(), Below).

%%%
%%%
%%%

handle_socket(Socket, Above) ->
    receive
	{Above, send, Data} ->
	    gen_tcp:send(Socket, [Data, "\n"]),
	    ?M:handle_socket(Socket, Above);
	{Above, send_error, Error} ->
	    send_error(Socket, Error),
	    ?M:handle_socket(Socket, Above);
	{Above, read_bytes, N} ->
	    inet:setopts(Socket, [{packet, raw}, {active, false}]),
	    {ok, Data} = gen_tcp:recv(Socket, N),
	    inet:setopts(Socket, [{packet, line}, {active, true}]),
	    Above ! {self(), bytes_read, Data},
	    ?M:handle_socket(Socket, Above);
	{Above, disconnect} ->
	    gen_tcp:close(Socket);
	{tcp_closed, Port} ->
	    io:format("Socket unexpectedly closed on port ~p~n", [Port]),
	    Above ! {self(), connection_closed};
	{tcp_error, Socket, Reason} ->
	    io:format("Socket error in ~p: ~s~n", [self(), Reason]),
	    gen_tcp:close(Socket),
	    Above ! {self(), connection_closed};
	{tcp, Socket, Data} ->
	    case string:tokens(Data, " \r\n") of
		[] -> []; % received empty line, do nothing
		[Command | Args] ->
		    case parse_arguments(Args) of
			error     -> send_error(Socket, [Command, ": Malformed argument(s)"]);
			Arguments -> Above ! {self(), Command, Arguments}
		    end
	    end,
	    ?M:handle_socket(Socket, Above);
	
	_Other ->
	    io:format("socket_handler got unexpected message: ~w~n", [_Other]),
	    ?M:handle_socket(Socket, Above)
    end.

send_error(Socket, Msg) ->
    gen_tcp:send(Socket, ["ERROR: ", Msg, "\n"]).

parse_arguments(Args) ->
    Integers = lists:map(fun(I) -> misc:read_integer(I) end, Args),
    case lists:all(fun(I) -> is_integer(I) end, Integers) of
	true  -> Integers;
	false -> error
    end.

%%%
%%% 
%%%

handle_messages(MsgQueue, Below) ->
    receive
	{event_handler, empty} ->
	    Below ! {self(), send, "NONEXISTENT"},
	    self() ! {self(), exhaust_queue},
	    ?M:handle_messages(MsgQueue, Below);
	{event_handler, {Pid, Action, Id}} ->
	    case Action of
		added   -> Msg = io_lib:format("ADDED ~B", [Id]);
		deleted -> Msg = io_lib:format("DELETED ~B", [Id])
	    end,
	    if Pid == self() ->
		    Below  ! {self(), send, Msg},
		    self() ! {self(), exhaust_queue},
		    ?M:handle_messages(queue:new(), Below);
	       true ->
		    ?M:handle_messages(queue:in(Msg, MsgQueue), Below)
	    end;
	{Pid, exhaust_queue} when Pid == self() ->
	    exhaust_queue(MsgQueue, fun(M) -> Below ! {self(), send, M} end),
	    ?M:handle_messages(queue:new(), Below);
	{Pid, disconnect} when Pid == self() ->
	    Below ! {self(), disconnect};
	{Below, connection_closed} ->
	    event_handler ! {self(), disconnect};
	{Below, Command, Args} ->
	    command(Command, Args, Below),
	    ?M:handle_messages(MsgQueue, Below);
	
	_Other ->
	    io:format("Client ~p got unexpected message: ~w~n", [self(), _Other]),
	    ?M:handle_messages(MsgQueue, Below)
    end.

exhaust_queue(Q, Fun) ->
    case queue:out(Q) of
	{{value, Item}, NewQ} ->
	    Fun(Item),
	    exhaust_queue(NewQ, Fun);
	{empty, _} -> []
    end.

%%%
%%%
%%%

-define(COMMANDS, ["ADDEVENT",
		   "GETEVENT",
		   "NUMRANGE",
		   "DELEVENT",
		   "DISCONNECT",
		   "POLL",
		   "SHUTDOWN"]).

command("ADDEVENT", [S, D, L], Below) ->
    Below ! {self(), send, "HIT ME"},
    Below ! {self(), read_bytes, L},
    receive
	{Below, bytes_read, Data} ->
	    event_handler ! {self(), add_event, #event{start=S, duration=D, text=Data}}
    end;

command("GETEVENT", [Event], Below) ->
    event_handler ! {self(), get_event, Event},
    receive
	{event_handler, empty} ->
	    Below ! {self(), send, "NONEXISTENT"};
	{event_handler, #event{start=S, duration=D, text=T}} ->
	    Below ! {self(), send, io_lib:format("~B ~B ~B~n~s", [S, D, string:len(T), T])}
    end;
%%self() ! {self(), exhaust_queue};

command("DELEVENT", [Event], _Below) ->
    event_handler ! {self(), delete_event, Event};

command("NUMRANGE", [S, L], Below) ->
    event_handler ! {self(), range, {S, L}},
    receive
	{event_handler, []}  -> Below ! {self(), send, "EMPTY"};
	{event_handler, IDs} -> Below ! {self(), send, misc:integers_to_string(IDs)}
    end;
%%self() ! {self(), exhaust_queue};

command("DISCONNECT", [], Below) ->
    event_handler ! {self(), disconnect},
    Below ! {self(), send, "BYE"},
    self() ! {self(), disconnect};

command("POLL", [], _Below) ->
    self() ! {self(), exhaust_queue};

%%% This should not be in the real program
command("SHUTDOWN", [], _Below) ->
    event_handler ! shutdown,
    self() ! {self(), disconnect};

command(Command, _, Below) ->
    case lists:member(Command, ?COMMANDS) of
	true  -> Below ! {self(), send_error, [Command, ": Wrong number of arguments"]};
	false -> Below ! {self(), send_error, ["Unknown command: ", Command]}
    end.
