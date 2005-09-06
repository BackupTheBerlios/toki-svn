#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "zeitc.h"

Zeitc *zeitc_new()
{
	Zeitc *zeitc;

	zeitc = malloc(sizeof(Zeitc));
	if(zeitc)
	{
		zeitc->sockfd = 0;

		zeitc->buf = malloc(sizeof(char)*ZEITC_BUFSIZE);
		if(!zeitc->buf)
		{
			free(zeitc);
			return NULL;
		}
	}

	return zeitc;
}

void zeitc_destroy(Zeitc *zeitc)
{
	free(zeitc->buf);
	free(zeitc);
}

int zeitc_connect(Zeitc *zeitc, char *host, int port)
{
	struct hostent *h;
   	struct sockaddr_in dest_addr;   // will hold the destination addr

   	if ((h=gethostbyname(host)) == NULL)
	{  // get the host info
		herror("gethostbyname");
		return -1;
   	}

   	zeitc->sockfd = socket(AF_INET, SOCK_STREAM, 0); // do some error checking!

   	dest_addr.sin_family = AF_INET;          // host byte order
   	dest_addr.sin_port = htons(port);   // short, network byte order
   	dest_addr.sin_addr.s_addr = inet_addr(inet_ntoa(*((struct in_addr *)h->h_addr)));
	memset(&(dest_addr.sin_zero), '\0', 8);  // zero the rest of the struct

   	// don't forget to error check the connect()!
	connect(zeitc->sockfd, (struct sockaddr *)&dest_addr, sizeof(struct sockaddr));

	return -1;
}

zeitc_event *zeitc_getevent(Zeitc *zeitc, int eventno)
{
	zeitc_event *event;
	char sendbuf[512];

	snprintf(sendbuf, 512, "GETEVENT %d\n", eventno);

	write(zeitc->sockfd, sendbuf, strlen(sendbuf));
	read(zeitc->sockfd, zeitc->buf, 512);
	printf("%s\n", zeitc->buf);

	return event;
}

void zeitc_event_destroy(zeitc_event *event)
{
}
