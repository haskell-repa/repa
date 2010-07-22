
LAPLACE_SRCS = \
	Main.c \
	../../libc/Matrix.c \
	../../libc/ColorRamp.c \
	../../libc/Timing.c

laplace : Main.c
	gcc --std=c99 -O3 $(LAPLACE_SRCS) -I../../libc -o laplace
	
clean :
	rm -f laplace