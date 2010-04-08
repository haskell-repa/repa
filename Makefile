TOPDIR = ..
BINARIES = raBench raTest saBench laplace mmult fft

# -----------------
raBench_DPH 	= raBench
raBench_SOURCES =  ReglibBench.hs Array.hs ArrayExamples.hs DArray.hs DArrayExamples.hs 

raBench_seq_FLAGS = -fsimplifier-phases=3 -package dph-base
raBench_par_FLAGS = -fsimplifier-phases=3 -package dph-base


# -----------------
raTest_DPH 	= raTest
raTest_SOURCES	=  ReglibTest.hs Array.hs ArrayExamples.hs DArray.hs DArrayExamples.hs 

raTest_seq_FLAGS = -fsimplifier-phases=3 -package dph-base
raTest_par_FLAGS = -fsimplifier-phases=3 -package dph-base



# -----------------
saBench_DPH 	= saBench
saBench_SOURCES =  ArrayBench.hs

saBench_seq_FLAGS = -fsimplifier-phases=3 -package dph-base
saBench_par_FLAGS = -fsimplifier-phases=3 -package dph-base



# ----------------
laplace_DPH	= laplace
laplace_SOURCES	= \
	examples/laplace/Main.hs \
	examples/laplace/SolveArray.hs \
	examples/laplace/SolveDArray.hs \
	examples/laplace/SolveCArray.hs \
	examples/laplace/SolveCArrayFlatDim.hs \
	examples/lib/PPM.hs \

laplace_seq_FLAGS = -iexamples/laplace -iexamples/lib -fsimplifier-phases=3 -package dph-base
laplace_par_FLAGS = -iexamples/laplace -iexamples/lib -fsimplifier-phases=3 -package dph-base


	

# -----------------
mmult_DPH	= mmult
mmult_SOURCES	= \
	examples/mmult/Main.hs
	
mmult_seq_FLAGS = -iexamples/mmult -iexamples/lib -fsimplifier-phases=3 -package dph-base
mmult_par_FLAGS = -iexamples/mmult -iexamples/lib -fsimplifier-phases=3 -package dph-base

# -----------------
fft_DPH		= fft
fft_SOURCES	= \
	examples/fft/Main.hs \
	examples/fft/FFTCArray.hs \
	examples/lib/StrictComplex.hs

fft_seq_FLAGS = -iexamples/fft -iexamples/lib
fft_par_FLAGS = -iexamples/fft -iexamples/lib

include $(TOPDIR)/mk/test.mk



# C versions --------------------------------------------------------------------------------------
laplacec_HEADER  = \
	examples/libc/Matrix.h \
	examples/libc/ColorRamp.h \
	examples/libc/Timing.h

laplacec_SOURCES = \
	examples/laplacec/Main.c \
	examples/libc/Matrix.c  \
	examples/libc/ColorRamp.c \
	examples/libc/Timing.c
	
seq/laplacec : $(laplacec_SOURCES) $(laplacec_HEADER)
	gcc -std=c99 -O2 -Iexamples/libc $(laplacec_SOURCES) -o seq/laplacec



mmultc_HEADER  = \
	examples/libc/Matrix.h \
	examples/libc/Timing.h

mmultc_SOURCES = \
	examples/mmultc/Main.c \
	examples/libc/Matrix.c \
	examples/libc/ColorRamp.c \
	examples/libc/Timing.c
		
seq/mmultc : $(mmultc_SOURCES) $(mmultc_HEADER)
	gcc -std=c99 -O2 -Iexamples/libc $(mmultc_SOURCES) -o seq/mmultc




