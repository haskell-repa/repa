
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "Vector.h"
#include "Timing.h"


void dotp(int lens, long* x1, long* y1, long* x2, long* y2, long* out)
{
	for (int i = 0; i != lens; ++i) {
		out[i] = (x1[i] * x2[i]) + (y1[i] * y2[i]);
	}
}

int main(int argc, char** argv)
{
	// Parse cmd line args.
	int	pointCount	= 0;

	if(argc == 2) {
		pointCount	= atoi(argv[1]);
	}
	else {
		printf("usage: quickhull <points>\n");
		exit(1);
	}

	long* x1		= malloc(pointCount * sizeof(long));
	long* y1		= malloc(pointCount * sizeof(long));
	long* x2		= malloc(pointCount * sizeof(long));
	long* y2		= malloc(pointCount * sizeof(long));

	long* out		= malloc(pointCount * sizeof(long));

	for (int i = 0; i < pointCount; i++) {
		x1[i] = i;
		y1[i] = i;
		x2[i] = i;
		y2[i] = i;
	}

	// Timing setup
        struct timeval start, finish;
        struct rusage start_ru, finish_ru;

        gettimeofday( &start, NULL );
        getrusage( RUSAGE_SELF, &start_ru );

	// Do the deed.
	dotp(pointCount, x1, y1, x2, y2, out);

	// Print how long it took.
        gettimeofday( &finish, NULL );
        getrusage( RUSAGE_SELF, &finish_ru );

//	printf("depth          = %d\n", depth);
//	printf("points on hull = %d\n", hull->length);

        sub_timeval( &finish, &start );
        sub_timeval( &finish_ru.ru_utime, &start_ru.ru_utime );
        sub_timeval( &finish_ru.ru_stime, &start_ru.ru_stime );
        add_timeval( &finish_ru.ru_utime, &finish_ru.ru_stime );

	printf("elapsedTimeMS   = ");
        print_timeval( &finish ); putchar( '\n' );

 	printf("cpuTimeMS       = ");
        print_timeval( &finish_ru.ru_utime); putchar( '\n' );
}

