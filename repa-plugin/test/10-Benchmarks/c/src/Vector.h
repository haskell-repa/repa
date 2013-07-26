
#ifndef _Vector_
#define _Vector_

#include <assert.h>

typedef struct {
	int	capacity;
	int	length;
	long*	x;
	long*	y;
} Vector;


static inline void vector_append(Vector* vector, long x, long y)
{
	assert(vector->length + 1 <= vector->capacity);
	
	int i		= vector->length;
	vector->x[i] 	= x;
	vector->y[i]	= y;
	vector->length++;
}


static inline Vector* vector_new(int capacity)
{
	Vector* vector	 = malloc(sizeof(Vector));
	vector->capacity = capacity;
	vector->length	 = 0;
	vector->x	 = malloc(sizeof(long) * capacity);
	vector->y	 = malloc(sizeof(long) * capacity);
	return vector;
}

static inline void vector_rewind(Vector* vector)
{
	vector->length = 0;
}


static inline void vector_delete(Vector* vector)
{
	free(vector->x);
	free(vector->y);
	free(vector);
}

#endif

