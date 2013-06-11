
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "Vector.h"
#include "Timing.h"

// FFS people.
#ifndef M_PI
#define M_PI 3.1415926535
#endif


extern int  quickHull	(Vector* points, Vector* hull);


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

	// Initialise the vector to hold the hull.
	Vector* hull		= vector_new(pointCount);
		
	// Use random points for test data.
	Vector* points		= vector_new(pointCount);

	for (int i = 0; i < pointCount; i++) {
        long x = i * (23489 * 5319) % (23489 * 978) % 500;
        long y = i * (12387 * 5319) % (12387 * 978) % 500;

		vector_append(points, x, y);
	}

/*
        let pts = gen 23489 sz `U.zip` gen 12387 sz
        pts' <- quickhull pts
        print pts'

-- incredibly dodgy number generator
gen :: Int -> Int -> U.Vector Int
gen seed size
 = U.generate size r
 where
  r i = i * (seed*5319) `mod` (seed * 978) `mod` 500
*/

	// Timing setup
        struct timeval start, finish;
        struct rusage start_ru, finish_ru;

        gettimeofday( &start, NULL );
        getrusage( RUSAGE_SELF, &start_ru );

	// Do the deed.
	int depth = quickHull (points, hull);

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
