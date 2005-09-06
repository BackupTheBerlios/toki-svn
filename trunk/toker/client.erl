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
	    gen_tcp:send(Socket, Data ++ "\n"),
	    ?M:handle_socket(Socket, Above);
	{Above, send_error, Error} ->
	    gen_tcp:send(Socket, io_lib:format("ERROR: ~s~n", [Error])),
	    ?M:handle_socket(Socket, Above);
	{Above, read_bytes, N} ->
	    inet:setopts(Socket, [{packet, raw}, {active, false}]),
	    {ok, Data} = gen_tcp:recv(Socket, N),
	    inet:setopts(Socket, [{packet, line}, {active, true}]),
	    Above ! {self(), bytes_read, Data},
	    ?M:handle_socket(Socket, Above);
	{Above, disconnect} ->
	    gen_tcp:close(Socket);
	{tcp, error, closed} ->
	    Above ! {self(), connection_closed},
	    gen_tcp:close(Socket);
	{tcp, Socket, Data} ->
	    Tokens = string:tokens(Data, " \r\n"),
	    case Tokens of
		[] -> []; % received empty line, do nothing
		_ ->
		    {Command, Arguments} = parse_command(Tokens),
		    case lists:member(Command, ?COMMANDS) of
			false ->
			    gen_tcp:send(Socket, io_lib:format("ERROR: Unknown command: ~s~n", [Command]));
			true when is_atom(Arguments) ->
			    gen_tcp:send(Socket, io_lib:format("ERROR: ~s: Malformed argument(s)~n", [Command]));
			true ->
			    Above ! {self(), Command, Arguments}
		    end
	    end,
	    ?M:handle_socket(Socket, Above);
	
	_Other ->
	    io:format("socket_handler got unexpected message: ~w~n", [_Other]),
	    ?M:handle_socket(Socket, Above)
    end.

parse_command([Operator | Args]) ->
    {list_to_atom(Operator), parse_arguments(Args)}.

parse_arguments(Args) ->
    case lists:splitwith(fun(I) -> is_integer(I) end, lists:map(fun(I) -> misc:read_integer(I) end, Args)) of
	{Integers, []} ->
	    Integers;
	_Other ->
	    error
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
		added ->
		    Msg = io_lib:format("ADDED ~B", [Id]);
		deleted ->
		    Msg = io_lib:format("DELETED ~B", [Id])
	    end,
	    if Pid == self() ->
		    Below ! {self(), send, Msg},
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

command('ADDEVENT', [S, D, L], Below) ->
    Below ! {self(), send, "HIT ME"},
    Below ! {self(), read_bytes, L},
    receive
	{Below, bytes_read, Data} ->
	    event_handler ! {self(), add_event, #event{start=S, duration=D, text=Data}}
    end;

command('GETEVENT', [Event], Below) ->
    event_handler ! {self(), get_event, Event},
    receive
	{event_handler, empty} ->
	    Below ! {self(), send, "NONEXISTENT"};
	{event_handler, #event{start=S, duration=D, text=T}} ->
	    Below ! {self(), send, io_lib:format("~B ~B ~B~n~s", [S, D, string:len(T), T])}
    end,
    self() ! {self(), exhaust_queue};

command('DELEVENT', [Event], _Below) ->
    event_handler ! {self(), delete_event, Event};

command('NUMRANGE', [S, L], Below) ->
    event_handler ! {self(), range, {S, L}},
    receive
	{event_handler, []} ->
	    Below ! {self(), send, "EMPTY"};
	{event_handler, IDs} ->
	    Below ! {self(), send, misc:integers_to_string(IDs)}
    end,
    self() ! {self(), exhaust_queue};

command('DISCONNECT', [], Below) ->
    event_handler ! {self(), disconnect},
    Below ! {self(), send, "BYE"},
    self() ! {self(), disconnect};

%%% This should not be in the real program
command('SHUTDOWN', [], _Below) ->
    event_handler ! shutdown,
    self() ! {self(), disconnect};

command(Command, _, Below) ->
    Below ! {self(), send_error, io_lib:format("~s: Wrong number of arguments", [Command])}.
