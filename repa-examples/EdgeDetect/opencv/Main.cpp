
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <opencv2/core/core.hpp>
#include <opencv2/core/mat.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/highgui/highgui.hpp>


int main(int argc, char *argv[])
{
	if(argc<2) {
		printf("Usage: main <image-file-name>\n");
		exit(0);
	}

	// Load source image.  
	IplImage* img 	= cvLoadImage(argv[1]);
	if(!img){
		printf("Could not load image file: %s\n",argv[1]);
		exit(0);
	}
	cv::Mat src	(img);
	int height	= src.rows;
	int width	= src.cols;
	int channels	= src.channels();
	

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
	cv::Mat dst = srcLum.clone();
	cv::Sobel (srcLum, dst
			, srcLum.depth()
			, 1		// xorder
			, 0		// yorder
			, 3		// kernel size
			, 1		// scale
			, 0		// delta
			, cv::BORDER_DEFAULT);


	// create a window
	cvNamedWindow("mainWin", CV_WINDOW_AUTOSIZE); 
	cvMoveWindow("mainWin", 100, 100);

	// show the image
	cv::imshow("mainWin", dst);

	// wait for a key
	cvWaitKey(0);

	// release the image
	cvReleaseImage(&img );
	return 0;
}

