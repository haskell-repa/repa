
MMULT_SRCS = \
	Main.c \
	../../libc/Matrix.c \
	../../libc/ColorRamp.c \
	../../libc/Timing.c

mmult : Main.c
	gcc --std=c99 -O3 $(MMULT_SRCS) -I../../libc -o mmult
	
clean :
	rm -f mmult