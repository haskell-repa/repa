
# == Dependencies ============================================================
#  * FFTW   >= 3.2.2
#  * OpenCV >= 2.1 

FFTW_INCLUDE	= /Users/benl/software/fft/fftw-3.2.2/include
FFTW_LIB	= /Users/benl/software/fft/fftw-3.2.2/lib/libfftw3.a

OPENCV_INCLUDE	= /usr/local/include
OPENCV_LIBDIR	= /usr/local/lib


# == Targets =================================================================
all : 	dist/build/repa-laplace-c/repa-laplace-c \
	dist/build/repa-highpass2d-fftw/repa-highpass2d-fftw \
	dist/build/repa-highpass3d-fftw/repa-highpass3d-fftw \
	dist/build/repa-mmult-c/repa-mmult-c \
	dist/build/repa-sobel-opencv/repa-sobel-opencv \
	dist/build/repa-canny-opencv/repa-canny-opencv

.PHONY : clean
clean : 
	rm -Rf dist
	

# -- Laplace ------------------------------------------------------------------
LAPLACE_SRCS = \
	examples/Laplace/src-c/Main.c \
	libc/Matrix.c \
	libc/ColorRamp.c \
	libc/Timing.c

dist/build/repa-laplace-c/repa-laplace-c : $(LAPLACE_SRCS)
	@echo "* Building Laplace"
	mkdir -p dist/build/repa-laplace-c
	gcc --std=c99 -O3 $+ -Ilibc -o $@
	@echo


# -- FFT/HighPass2D ----------------------------------------------------------
HIGHPASS2D_SRCS = \
	examples/FFT/HighPass2D/src-fftw/Main.c \
	examples/FFT/HighPass2D/src-fftw/FFTW.c \
	examples/FFT/HighPass2D/src-fftw/Jones.c \
	libc/BMP.c 

dist/build/repa-highpass2d-fftw/repa-highpass2d-fftw : $(HIGHPASS2D_SRCS)
	@echo "* Building Highpass2D"
	mkdir -p dist/build/repa-highpass2d-fftw
	gcc --std=c99 -O3 $+ $(FFTW_LIB) -Ilibc -I$(FFTW_INCLUDE) -o $@
	@echo


# -- FFT/HighPass3D -----------------------------------------------------------
HIGHPASS3D_SRCS = \
	examples/FFT/HighPass3D/src-fftw/Main.c

dist/build/repa-highpass3d-fftw/repa-highpass3d-fftw : $(HIGHPASS3D_SRCS)
	@echo "* Building Highpass3D"
	mkdir -p dist/build/repa-highpass3d-fftw
	gcc --std=c99 -O3 $+ $(FFTW_LIB) -Ilibc -I$(FFTW_INCLUDE) -o $@
	@echo


# -- MMult --------------------------------------------------------------------
MMULT_SRCS = \
	examples/MMult/src-c/Main.c \
	libc/Matrix.c \
	libc/ColorRamp.c \
	libc/Timing.c

dist/build/repa-mmult-c/repa-mmult-c : $(MMULT_SRCS)
	@echo "* Building MMult"
	mkdir -p dist/build/repa-mmult-c
	gcc --std=c99 -O3 $+ -Ilibc -o $@
	@echo
	

# -- Sobel --------------------------------------------------------------------
SOBEL_SRCS = \
	examples/Sobel/src-opencv/Main.cpp \
	libc/Timing.c 
	
dist/build/repa-sobel-opencv/repa-sobel-opencv : $(SOBEL_SRCS)
	@echo "* Building Sobel"
	mkdir -p dist/build/repa-sobel-opencv
	g++ -O3 $+ -Ilibc -I$(OPENCV_INCLUDE) -L$(OPENCV_LIBDIR) -lopencv_core -lopencv_highgui -lopencv_imgproc -o $@


# -- Canny ---------------------------------------------------------------
CANNY_SRCS = \
	examples/Canny/src-opencv/Main.cpp \
	libc/Timing.c 
	
dist/build/repa-canny-opencv/repa-canny-opencv : $(CANNY_SRCS)
	@echo "* Building Canny"
	mkdir -p dist/build/repa-canny-opencv
	g++ -O3 $+ -Ilibc -I$(OPENCV_INCLUDE) -L$(OPENCV_LIBDIR) -lopencv_core -lopencv_highgui -lopencv_imgproc -o $@

