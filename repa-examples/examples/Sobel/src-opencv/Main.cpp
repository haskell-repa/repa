
// Convert an RGB image to greyscale and apply the X&Y Sobel operators.
// Produce an image containing the magnitude of the vector at each point.
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <opencv2/core/core.hpp>
#include <opencv2/core/mat.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>

#include "Timing.h"

int main(int argc, char *argv[])
{
	if(argc != 4) {
		printf("Usage: sobel <iterations> <in.bmp> <out.bmp>\n");
		exit(0);
	}
	int	iterations	= atoi(argv[1]);
	char*	fileNameIn	= argv[2];
	char*	fileNameOut	= argv[3];

	// Load source image.  
	cv::Mat src	= cv::imread(fileNameIn);
	if(src.data == NULL) {
		printf("Could not load image file: %s\n", fileNameIn);
		exit(0);
	}
	int height	= src.rows;
	int width	= src.cols;
	int channels	= src.channels();
	assert (channels == 3);

	
	// Get luminance of source image as floats.
	cv::Mat srcLum	(src.rows, src.cols, CV_32F);
	for(int i = 0; i < height; i++) {
		uchar* rowSrc		= src.ptr(i);
		float* rowSrcLum	= (float*)srcLum.ptr(i);
		
		for(int j = 0; j < width; j++) {
			uchar r	= rowSrc[j * channels + 0];
			uchar g = rowSrc[j * channels + 1];
			uchar b = rowSrc[j * channels + 2];
			
			rowSrcLum[j] = ((r * 0.3) + (g * 0.59) + (b * 0.11)) / 255.0;
		}
	}

	// Compute Sobel of source luminance.
	cv::Mat sobelX = srcLum.clone();
	cv::Mat sobelY = srcLum.clone();
	
	struct benchtime *bt = bench_begin();

	for(int iters = 0; iters < iterations; iters++) {
	cv::Sobel (srcLum, sobelX
			, srcLum.depth()
			, 1		// xorder
			, 0		// yorder
			, 3		// kernel size
			, 1		// scale
			, 0		// delta
			, cv::BORDER_DEFAULT);

	cv::Sobel (srcLum, sobelY
			, srcLum.depth()
			, 0		// xorder
			, 1		// yorder
			, 3		// kernel size
			, 1		// scale
			, 0		// delta
			, cv::BORDER_DEFAULT);
	}
	bench_done(bt);


	// Create output greyscale image.
	//   The imwrite function doesn't handle float data.
	cv::Mat matOut (src.rows, src.cols, CV_8U);

	for(int i = 0; i < height; i++) {
		float* rowSobelX	= (float*)sobelX.ptr(i);
		float* rowSobelY	= (float*)sobelY.ptr(i);
		uchar* rowOut		= matOut.ptr(i);

		for(int j = 0; j < width; j++) {
			float sX	= rowSobelX[j];
			float sY	= rowSobelY[j];

			rowOut[j]	= sqrt(sX * sX + sY * sY) * 100;
		}
	}

	// Write out the data to a new image.
	cv::imwrite(fileNameOut, matOut);
	
	return 0;
}

