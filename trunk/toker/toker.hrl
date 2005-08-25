% toker include file

-define(M, ?MODULE). % hope this works

-record(event, {start, duration, text}).

-define(DIR, "/tmp/kalender/").

-define(PORT, 8000).

