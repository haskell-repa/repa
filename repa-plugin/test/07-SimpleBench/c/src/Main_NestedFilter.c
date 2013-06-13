
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "Vector.h"
#include "Timing.h"


void nested(long lens, long* uvec, long* out1, long* out2, long* out1_len, long* out2_len)
{
	long len1 = 0;
	long len2 = 0;
	for (long i = 0; i != lens; ++i) {
		long x = uvec[i];
		if (x > 50) {
			out1[len1++] = x;
			if (x < 100) {
				out2[len2++] = x;
			}
		}
	}
	*out1_len = len1;
	*out2_len = len2;
}
/*
 = let ys  = U.filter (\x -> x > 50) xs
       zs  = U.filter (\x -> x < 100) ys
   in  (ys, zs)
*/

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

	long* out1		= malloc(pointCount * sizeof(long));
	long* out2		= malloc(pointCount * sizeof(long));

	for (int i = 0; i < pointCount; i++) {
		uvec[i] = i;
	}

	// Timing setup
        struct timeval start, finish;
        struct rusage start_ru, finish_ru;

        gettimeofday( &start, NULL );
        getrusage( RUSAGE_SELF, &start_ru );

	// Do the deed.
	long out1_len, out2_len;
	nested(pointCount, uvec, out1, out2, &out1_len, &out2_len);

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

