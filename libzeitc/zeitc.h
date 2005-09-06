#ifndef LIBZEITC_H
#define LIBZEITC_H

#include <time.h>

#define ZEITC_BUFSIZE 4096

typedef struct zeitc_struct
{
	int sockfd;
	char *buf; /* recieve buffer */
} Zeitc;

typedef struct zeitc_event_struct
{
	time_t start;
	time_t duration;
	char *data;
} zeitc_event;

Zeitc *zeitc_new();
void zeitc_destroy(Zeitc *zeitc);
int zeitc_connect(Zeitc *zeitc, char *host, int port);
zeitc_event *zeitc_getevent(Zeitc *zeitc, int eventid);
void zeitc_event_destroy(zeitc_event *event);

/*c| ADDEVENT starttime duration textlength\n
c| NUMRANGE starttime duration\n
c| GETEVENT <num>\n
c| DELEVENT <num>\n
c| NOP

When new events are available on server:
s| ADDED <num>\n*/

#endif
