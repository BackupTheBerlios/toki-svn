% toker include file

-define(M, ?MODULE). % hope this works

-record(event, {start, duration, text}).

-define(DIR, "/tmp/kalender/").

-define(DBFILE, "/tmp/eventdb").

-define(PORT, 8745).

-define(COMMANDS, ['ADDEVENT',
		   'GETEVENT',
		   'NUMRANGE',
		   'DELEVENT',
		   'DISCONNECT', 'SHUTDOWN']).
