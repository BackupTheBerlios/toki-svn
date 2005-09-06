#include <stdio.h>
#include <unistd.h>

#include "zeitc.h"

int main()
{
	Zeitc *zeitc;

	zeitc = zeitc_new();
	zeitc_connect(zeitc, "localhost", 8745);
	zeitc_getevent(zeitc, 1);
	zeitc_getevent(zeitc, 2);
	zeitc_getevent(zeitc, 3);
	zeitc_getevent(zeitc, 4);
	zeitc_getevent(zeitc, 5);
	zeitc_getevent(zeitc, 6);

	return 0;
}
