
HEADERS 	= 
SOURCES 	= Main.c
FFTW_INCLUDE	= /Users/benl/software/fft/fftw-3.2.2/include
FFTW_LIB	= /Users/benl/software/fft/fftw-3.2.2/lib/libfftw3.a

highpass : $(SOURCES) $(HEADERS)
	gcc --std=c99 -O3 $(SOURCES) $(FFTW_LIB) -I$(FFTW_INCLUDE) -o highpass
