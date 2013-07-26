
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "Vector.h"
#include "Timing.h"


void filtermax(long size, long* uvec, long* out, long* out_len, long* out_max)
{
        assert (size > 0);
        
	long len       = 0;
	long max       = uvec[0] + 1;

	for (long i = 0; i < size; i++) {
		long s2 = uvec[i] + 1;
		if (s2 > 0) {
			out[len++] = s2;
			if (s2 > max) {
				max = s2;
			}
		}
	}
	*out_len = len;
	*out_max = max;
}


int main(int argc, char** argv)
{
	// Parse cmd line args.
	int	size	      = 0;

	if(argc == 2) {
		size          = atoi(argv[1]);
	}
	else {
		printf("usage: filtermax <size>\n");
		exit(1);
	}

        // Input vector.
	long* uvec     = malloc(size * sizeof(long));

        // 
        for (int i = 0; i < size; i++) {
                uvec[i] = i;
        }

        // Output vector.
	long* out      = malloc(size * sizeof(long));

	// Timing setup
        struct timeval start, finish;
        struct rusage start_ru, finish_ru;

        gettimeofday( &start, NULL );
        getrusage( RUSAGE_SELF, &start_ru );

	// Do the deed.
	long out_len, out_max;
	filtermax(size, uvec, out, &out_len, &out_max);

	// Print how long it took.
        gettimeofday( &finish, NULL );
        getrusage( RUSAGE_SELF, &finish_ru );

        sub_timeval( &finish, &start );
        sub_timeval( &finish_ru.ru_utime, &start_ru.ru_utime );
        sub_timeval( &finish_ru.ru_stime, &start_ru.ru_stime );
        add_timeval( &finish_ru.ru_utime, &finish_ru.ru_stime );

	printf("elapsedTimeMS   = ");
        print_timeval( &finish ); putchar( '\n' );

 	printf("cpuTimeMS       = ");
        print_timeval( &finish_ru.ru_utime); putchar( '\n' );
}

