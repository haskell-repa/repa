
all: quickhull dotproduct filtermax filtersum mapmap nestedfilter

quickhull  : Main_QuickHull.c QuickHull.c Timing.c Timing.h Vector.h
	gcc --std=c99 -O2 -o quickhull Main_QuickHull.c QuickHull.c Timing.c

dotproduct : Main_DotProduct.c Timing.c Timing.h Vector.h
	gcc --std=c99 -O2 -o dotproduct Main_DotProduct.c Timing.c

filtermax : Main_FilterMax.c Timing.c Timing.h Vector.h
	gcc --std=c99 -O2 -o filtermax Main_FilterMax.c Timing.c

filtersum : Main_FilterSum.c Timing.c Timing.h Vector.h
	gcc --std=c99 -O2 -o filtersum Main_FilterSum.c Timing.c

mapmap    : Main_MapMap.c Timing.c Timing.h Vector.h
	gcc --std=c99 -O2 -o mapmap Main_MapMap.c Timing.c

nestedfilter: Main_NestedFilter.c Timing.c Timing.h Vector.h
	gcc --std=c99 -O2 -o nestedfilter Main_NestedFilter.c Timing.c
