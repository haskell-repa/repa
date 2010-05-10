
#include <sys/types.h>
#include <stdlib.h>
#include <stdio.h>
#include "BMP.h"


extern void fftw_highpass2d(int width, int height, u_int8_t* image);


int main(int argc, char** argv)
{
	if(argc != 3) {
		printf("usage: highpass <input.bmp> <output.bmp>\n");
		exit(1);
	}
	
	char* fileNameIn	= argv[1];
	char* fileNameOut	= argv[2];
	
	ImageRGB* image	= readBMP24(fileNameIn);

	fftw_highpass2d(image->width, image->height, image->red);
	fftw_highpass2d(image->width, image->height, image->green);
	fftw_highpass2d(image->width, image->height, image->blue);
	
	writeBMP24(fileNameOut, image);
}
