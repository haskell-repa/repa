
all: quickhull dotproduct filtermax filtersum mapmap nestedfilter

quickhull  : src/Main_QuickHull.c src/QuickHull.c src/Timing.c src/Timing.h src/Vector.h
	gcc --std=c99 -O3 -o bin/quickhull src/Main_QuickHull.c src/QuickHull.c src/Timing.c

dotproduct : src/Main_DotProduct.c src/Timing.c src/Timing.h src/Vector.h
	gcc --std=c99 -O3 -o bin/dotproduct src/Main_DotProduct.c src/Timing.c

filtermax : src/Main_FilterMax.c src/Timing.c src/Timing.h src/Vector.h
	gcc --std=c99 -O3 -o bin/filtermax src/Main_FilterMax.c src/Timing.c

filtersum : src/Main_FilterSum.c src/Timing.c src/Timing.h src/Vector.h
	gcc --std=c99 -O3 -o bin/filtersum src/Main_FilterSum.c src/Timing.c

mapmap    : src/Main_MapMap.c src/Timing.c src/Timing.h src/Vector.h
	gcc --std=c99 -O3 -o bin/mapmap src/Main_MapMap.c src/Timing.c

nestedfilter: src/Main_NestedFilter.c src/Timing.c src/Timing.h src/Vector.h
	gcc --std=c99 -O3 -o bin/nestedfilter src/Main_NestedFilter.c src/Timing.c


.PHONY : clean
clean: 
	@rm -f bin/*
