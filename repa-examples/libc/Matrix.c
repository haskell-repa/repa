
#include <stdio.h>
#include <stdlib.h>
#include "Matrix.h"
#include "ColorRamp.h"


// Matrix Creation and Freeing --------------------------------------------------------------------
// Given a function that produces each element, 
//	create a matrix of a given size.
Matrix* createMatrix
	( int width
	, int height
	, double (*mkElem)(int width, int height, int x, int y))
{
	double** data	= malloc (sizeof(double*) * height);

	for (int y = 0; y < height; y++) {
		data[y]	= malloc (sizeof(double) * width);
		
		for (int x = 0; x < width; x++)
			data[y][x] = mkElem(width, height, x, y);
	}
	
	Matrix* mat	= malloc (sizeof(Matrix));
	mat->width	= width;
	mat->height	= height;
	mat->data	= data;
	return mat;
}


void freeMatrix (Matrix* mat)
{
	for (int y = 0; y < mat->height; y++)
		free(mat->data[y]);
	
	free(mat->data);
	free(mat);
}


// Check whether these matrices have the same width and height.
int matricesHaveSameShape (Matrix* mat1, Matrix* mat2)
{
	return	(mat1->width  == mat2->width)
	    &&  (mat1->height == mat2->height);
}



// Random Matrices --------------------------------------------------------------------------------
double mkRandom (int width, int height, int x, int y)
{
	long xv	= random();
	long yv	= random();
	return	(double)xv / (double) yv;
}


Matrix* newRandomMatrix (int width, int height)
{
	return createMatrix (width, height, mkRandom);
}


// Zero Matrices ----------------------------------------------------------------------------------
double mkZero (int width, int height, int x, int y)
{	
	return 0;
}

Matrix* newZeroMatrix (int width, int height)
{
	return createMatrix (width, height, mkZero);
}


// Computations -----------------------------------------------------------------------------------
double	sumMatrix
	(Matrix* mat)
{
	double	totalSum	= 0;
	for(int j = 0; j < mat->height; j++) {
		double rowSum	= 0;
		for(int i = 0; i < mat->width; i++)
			rowSum	+= mat->data[j][i];
			
		totalSum	+= rowSum;
	}
	
	return totalSum;
}


// PPM --------------------------------------------------------------------------------------------
void writeMatrixAsPPM
	( char*  	fileName
	, Matrix* 	mat )
{
	FILE* file	= fopen(fileName, "w+");
	fprintf(file, "P3\n");
	fprintf(file, "%d %d\n", mat->width, mat->height);
	fprintf(file, "255\n");
	
	for (int y = 0; y < mat->height; y++)
	for (int x = 0; x < mat->width; x++) {
		double v = mat->data[y][x];

		double r = 0;
		double g = 0;
		double b = 0;
		rampColorHotToCold(v, 0, 180, &r, &g, &b);

		fprintf	( file
			, "%d %d %d\n"
			, (int)(r * 255)
			, (int)(g * 255)
			, (int)(b * 255) );		
	}

	fclose(file);
}


void writeMatrixAsTextFile
	( char*		fileName
	, Matrix*	mat)
{
	FILE* file	= fopen(fileName, "w+");
	fprintf(file, "MATRIX\n");
	fprintf(file, "%d %d\n", mat->width, mat->height);
	
	for (int y = 0; y < mat->height; y++)
	for (int x = 0; x < mat->width; x++) {
		double v = mat->data[y][x];
		
		fprintf (file, "%.14f\n", v);	
	}
	
}














