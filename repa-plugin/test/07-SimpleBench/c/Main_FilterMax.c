
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "Vector.h"
#include "Timing.h"


void filtermax(long lens, long* uvec, long* out, long* out_len, long* out_max, long* out_min)
{
	// ASSERT lens /= 0 ?
	long len = 0;
	long max = uvec[0]+1;
	long min = uvec[0]+1;
	for (long i = 0; i != lens; ++i) {
		long s2 = uvec[i] + 1;
		if (s2 > 0) {
			out[len++] = s2;
			if (s2 > max) {
				max = s2;
			}
			if (s2 < min) {
				min = s2;
			}
		}
	}
	*out_len = len;
	*out_max = max;
	*out_min = min;
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

	long* uvec		= malloc(pointCount * sizeof(long));

	long* out		= malloc(pointCount * sizeof(long));

	for (int i = 0; i < pointCount; i++) {
		uvec[i] = i;
	}

	// Timing setup
        struct timeval start, finish;
        struct rusage start_ru, finish_ru;

        gettimeofday( &start, NULL );
        getrusage( RUSAGE_SELF, &start_ru );

	// Do the deed.
	long out_len, out_max, out_min;
	filtermax(pointCount, uvec, out, &out_len, &out_max, &out_min);

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

