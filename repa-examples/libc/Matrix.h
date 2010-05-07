
#ifndef _Matrix_h_
#define _Matrix_h_

// A matrix represented as an array of rows.
typedef struct {
	int 	 width;
	int 	 height;
	double** data;
} Matrix;


Matrix* createMatrix
	( int width
	, int height
	, double (*mkElem)(int width, int height, int x, int y));
	
void freeMatrix 
	(Matrix* mat);

Matrix* newRandomMatrix
	( int width
	, int height);

Matrix* newZeroMatrix
	( int width
	, int height);
	
int matricesHaveSameShape 
	( Matrix* mat1
	, Matrix* mat2);

double	sumMatrix
	(Matrix* mat);

void writeMatrixAsPPM
	( char*  	fileName
	, Matrix* 	mat );

void writeMatrixAsTextFile
	( char*		fileName
	, Matrix*	mat);
	

#endif
