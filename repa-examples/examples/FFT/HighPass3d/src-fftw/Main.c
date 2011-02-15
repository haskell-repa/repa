

#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fftw3.h>
#include <math.h>


void highpass3d_fftw(int depth, int width, int height, u_int8_t* image);


int main(int argc, char** argv)
{
	if(argc != 1) {
		printf("usage: highpass size\n");		
		exit(1);
	}

	// The size of the whole image.
	int len		= 128;
	int size	= len * len * len;

	// Create the initial image.
	u_int8_t* image	= (u_int8_t*) malloc(size);
	memset(image, 0, size);

	// Do high pass filtering.
	highpass3d_fftw(len, len, len, image);
	
	// Cleanup.
	free(image);
}

void highpass3d_fftw(int depth, int width, int height, u_int8_t* image)
{
	// The size of the whole image.
	int size		= depth * height * width;

	// Allocate input and output buffers
	fftw_complex* buf1	= (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * size);
	fftw_complex* buf2	= (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * size);

	// Copy in image data as real values for the transform.
	for(int i = 0; i < size; i++) {
		buf1[i][0]	= (double)image[i];
		buf1[i][1]	= 0;
	}

	// Transform to frequency space.
	fftw_plan pFwd = fftw_plan_dft_3d(width, height, depth, buf1, buf2, FFTW_FORWARD, FFTW_ESTIMATE);
	fftw_execute(pFwd);
	fftw_destroy_plan(pFwd);

	// Zap the DC value
	buf2[0][0]	= 0;
	buf2[0][1]	= 0;

	// Transform back to image space.
	fftw_plan pBack	= fftw_plan_dft_3d(width, height, depth, buf2, buf1, FFTW_BACKWARD, FFTW_ESTIMATE);
	fftw_execute(pBack);
	fftw_destroy_plan(pBack);
	
	// Have to scale the output values to get back to the original.
	for(int i = 0; i < size; i++) {
		buf1[i][0]	= buf1[i][0] / size;
		buf1[i][1]	= buf1[i][1] / size;
	}
	
	// Copy the magnitude of the result back into the image.
	for(int i = 0; i < size; i++) {
		double re	= buf1[i][0];
		double im	= buf1[i][1];
		double mag	= sqrt (re*re + im*im);
		image[i]	= (u_int8_t)mag;
	}

	// Cleanup.
	fftw_free(buf1); 
	fftw_free(buf2);
}
