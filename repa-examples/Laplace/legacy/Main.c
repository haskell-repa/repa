
// Naive solver for the Laplace equation.
//	Boundary conditions are fixed on the edge of the square.
//
//	This method is very slow to converge for large matrices.
//	If we were going to do it properly we'd start with a matrix a fraction of the size
//	of the final result, solve that, then use those values to tile a larger initial 
//	matrix, solve that etc.. working our way up to the final result. Doing this would
//	help propagate information from the boundary conditions throughout the matrix.
//
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <Matrix.h>
#include <Timing.h>


// Boundary Conditions ----------------------------------------------------------------------------
// Make the mask for the boundary conditions.
//	Should return 0 when the point is part of the boundary, and 1 otherwise.
double mkBoundaryMask (int width, int height, int x, int y)
{
	int w 	= width  - 1;
	int h	= height - 1;
	
	if      (x == 0)			return 0;
	else if (y == 0)			return 0;
	else if (x >= w)			return 0;
	else if (y >= h)			return 0;
	else					return 1;
}


// Make the values for the boundary conditions.
//	Should return 0 where the point is not part of the boundary.
double mkBoundaryValue (int width, int height, int x, int y)
{
	int w 	= width  - 1;
	int h	= height - 1;
	
	if 	(x == 0 && y > 0 && y < h)	return 80;
	else if (y == 0 && x > 0 && x < w)	return 20;
	else if (x == w && y > 0 && y < h)	return 0;
	else if	(y == h && x > 0 && x < w)	return 180;
	else					return 0;
}


// Apply the boundary conditions to this matrix.
//	The mask  matrix has 0 in places where boundary conditions hold
//	and 1 otherwise.
//
//	The value matrix has the boundary condition value in places where it holds,
//	and 0 otherwise.
// 
void applyBoundary
	( Matrix* matDest
	, Matrix* matBoundMask
	, Matrix* matBoundValue)
{
	assert(matricesHaveSameShape(matDest, matBoundMask));
	assert(matricesHaveSameShape(matDest, matBoundValue));

	for (int y = 0; y < matDest->height; y++)
	for (int x = 0; x < matDest->width; x++) {
		matDest->data[y][x]
			= (matDest->data[y][x] * matBoundMask->data[y][x]) 
			+ matBoundValue->data[y][x];
	}
}
		

// Relaxation -------------------------------------------------------------------------------------
// Perform one relaxation cycle with a four point stencil for the Laplace equation.
void relaxLaplace 
	( Matrix* matDest
	, Matrix* matSrc)
{
	assert(matricesHaveSameShape(matDest, matSrc));
	
	for (int y = 1; y < matDest->height - 1; y++) 
	for (int x = 1; x < matDest->width  - 1; x++) {
		double left	= matSrc->data[y]  [x-1];
		double right	= matSrc->data[y]  [x+1];
		double up	= matSrc->data[y+1][x];
		double down	= matSrc->data[y-1][x];
		
		matDest->data[y][x] = (left + right + up + down) / 4;
	}	
}


// Solver -----------------------------------------------------------------------------------------
// Main solver loop.
//	Relax the matrix, then apply boundary conditions, for some number of iterations.
//	The values for the next iteration are written to matDest, 
//	then the matInitial and matDest buffers are swapped. 
//
//	Returns either matInitial or matDest, depending on how many iterations we took.
//
Matrix* solve
	( int iterations
	, Matrix* matBoundMask
	, Matrix* matBoundValue
	, Matrix* matInitial
	, Matrix* matDest)	// Where to write the result of the first iteration.
{
	assert(matricesHaveSameShape(matDest, matInitial));
	assert(matricesHaveSameShape(matDest, matBoundValue));
	assert(matricesHaveSameShape(matDest, matBoundMask));

	Matrix* matTmp	= 0;

	for (int i = 0; i < iterations; i++) {
		relaxLaplace  (matDest, matInitial);
		applyBoundary (matDest, matBoundMask, matBoundValue);

		matTmp		= matDest;
		matDest		= matInitial;
		matInitial	= matTmp;
	}

	// Return result of last iteration.
	return	matTmp;
}	
	

// Main -------------------------------------------------------------------------------------------
int main(int argc, char** argv)
{
	// Argument parsing
	if (argc != 5) {
		printf("Usage: laplace <width> <height> <iterations> <output file.ppm>\n");
		printf("  width, height  :: Int      The width and height of the matrix\n");
		printf("  iterations     :: Int      Number of iterations to use in the solver\n");
		exit(0);
	}
	int width	= 0;
	int height	= 0;
	int iterations	= 0;
	
	if(sscanf(argv[1], "%d", &width) != 1) {
		printf("laplace: can't parse matrix width\n");
		exit(1);
	}

	if(sscanf(argv[2], "%d", &height) != 1) {
		printf("laplace: can't parse matrix height\n");
		exit(1);
	}

	if(sscanf(argv[3], "%d", &iterations) != 1) {
		printf("laplace: can't parse iterations\n");
		exit(1);
	}
		
	char* fileName	= argv[4];

	
	// Setup boundary condition matrices
	Matrix*	matBoundMask	= createMatrix (width, height, mkBoundaryMask);
	Matrix*	matBoundValue	= createMatrix (width, height, mkBoundaryValue);	
	
	// Set the initial matrix to the same as the boundary conditions.
	Matrix*	matInitial	= createMatrix (width, height, mkBoundaryValue);
	
	// A destination buffer, to write the next iteration into.
	Matrix* matDest		= createMatrix (width, height, mkBoundaryValue);
	
	// Run the solver.
	//	The result is either the matInitial or matBuffer, depending
	//	on how many iterations we took.
        struct timeval start, finish;
        struct rusage start_ru, finish_ru;

        gettimeofday( &start, NULL );
        getrusage( RUSAGE_SELF, &start_ru );

	Matrix* matFinal	
		= solve ( iterations
			, matBoundMask, matBoundValue
			, matInitial, matDest);

        gettimeofday( &finish, NULL );
        getrusage( RUSAGE_SELF, &finish_ru );

	// Write the output to a PPM file.
	writeMatrixAsPPM(fileName, matFinal);
	
	// Cleanup
	freeMatrix (matBoundMask);
	freeMatrix (matBoundValue);
	freeMatrix (matInitial);
	freeMatrix (matDest);

        sub_timeval( &finish, &start );
        sub_timeval( &finish_ru.ru_utime, &start_ru.ru_utime );
        sub_timeval( &finish_ru.ru_stime, &start_ru.ru_stime );
        add_timeval( &finish_ru.ru_utime, &finish_ru.ru_stime );

        print_timeval( &finish ); putchar( '/' );
        print_timeval( &finish_ru.ru_utime); putchar( '\n' );
}

