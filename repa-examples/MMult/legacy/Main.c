
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <Matrix.h>
#include <Timing.h>


void mmult
	( Matrix* matDest
	, Matrix* matSrc1
	, Matrix* matSrc2)
{
	int dim1	= matSrc1->width;
	int dim2	= matSrc2->height;
	int dim3	= matDest->width;

	for (int i = 0; i < dim1; i++)
	{
		for (int j = 0; j < dim3; j++)
			matDest->data[i][j] = 0.;

		for (int k = 0; k < dim2; k++)
			for (int j = 0; j < dim3; j++)
				matDest->data[i][j] 
					+= matSrc1->data[i][k] 
					*  matSrc2->data[k][j];
	}	
}


// Main -------------------------------------------------------------------------------------------
void badUsage()
{
	printf("Usage: mmultc [args..]\n\n");
	printf("  -random    <height> <width>   Use a random input matrix of this size.\n");
	printf("  -dumpinput                    Dump input matrices used.\n");
	printf("  -out       <filename>         Write resulting matrix to this file.\n");
	printf("\n");
	printf("  You must specify two input matrices (with -random)\n");
	printf("\n");
	exit(0);	
}

int main(int argc, char** argv)
{
	// Argument parsing.
	Matrix*	mat[2];
	int 	matNum			= 0;
	int	dumpInputMatrices	= 0;
	char*	outFileName		= 0;
	
	for(int a = 1; a < argc; ) {
		if(   (strcmp(argv[a], "-random") == 0) 
		   && (a + 2  < argc)
		   && (matNum < 2))
		{
			a++;
			
			int width	= 0;
			int height	= 0;

			if(sscanf(argv[a++], "%d", &width) != 1) {
				printf("laplace: can't parse matrix width\n");
				exit(1);
			}

			if(sscanf(argv[a++], "%d", &height) != 1) {
				printf("laplace: can't parse matrix height\n");
				exit(1);
			}
			
			mat[matNum++]	= newRandomMatrix (width, height);
		}
		else if (  (strcmp(argv[a], "-out") == 0)
			&& (a + 1 < argc))
		{
			a++;
			outFileName	= argv[a++];
		}
		else if (  strcmp(argv[a], "-dumpinput") == 0)
		{
			a++;
			dumpInputMatrices	= 1;
		}

		else	badUsage();
	}

	if (matNum != 2)
		badUsage();


	// Alloc the destination matrix.
	Matrix* matDest	= newZeroMatrix   (mat[1]->width, mat[0]->height);
	
	// Do the dead.
	struct timeval start, finish;
        struct rusage start_ru, finish_ru;

        gettimeofday( &start, NULL );
        getrusage( RUSAGE_SELF, &start_ru );

	mmult(matDest, mat[0], mat[1]);

        gettimeofday( &finish, NULL );
        getrusage( RUSAGE_SELF, &finish_ru );


	// Write out matrices as files, if we were asked for them
	if (dumpInputMatrices) {
		char name[80];

		snprintf(name, 80, "input1-%dx%d.mat", mat[0]->width, mat[0]->height);
		writeMatrixAsTextFile(name, mat[0]);

		snprintf(name, 80, "input2-%dx%d.mat", mat[1]->width, mat[1]->height);
		writeMatrixAsTextFile(name, mat[1]);
	}

	if(outFileName != 0)
		writeMatrixAsTextFile(outFileName, 	matDest);


	// Dump timing info.
        sub_timeval( &finish, &start );
        sub_timeval( &finish_ru.ru_utime, &start_ru.ru_utime );
        sub_timeval( &finish_ru.ru_stime, &start_ru.ru_stime );
        add_timeval( &finish_ru.ru_utime, &finish_ru.ru_stime );

        print_timeval( &finish ); putchar( '/' );
        print_timeval( &finish_ru.ru_utime); putchar( '\n' );

	// Dump checksum
	printf("sum = %f\n", sumMatrix(matDest));
}



