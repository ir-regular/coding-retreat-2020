#include <float.h>
#include <stdlib.h>
#include "mc_gen.h"

// Notes on values generated:

// 0 <= random() <= RAND_MAX
// RAND_MAX == INT_MAX < UINT_MAX < DBL_MAX

unsigned *mc_gen_rand_unsigned(size_t len)
{
	unsigned *list = malloc(len * sizeof(unsigned));

	if (list) {
                mc_init_rand_unsigned(len, list);
	}

	return list;
}

void mc_init_rand_unsigned(size_t len, unsigned list[len])
{
	for (size_t i = 0; i < len; i++) {
		// by definition is unsigned
		// lowest value random() returns is 0
		list[i] = random();
	}
}

int *mc_gen_rand_int(size_t len)
{
	int *list = malloc(len * sizeof(int));

	if (list) {
		mc_init_rand_int(len, list);
	}

	return list;
}

void mc_init_rand_int(size_t len, int list[len])
{
        for (size_t i = 0; i < len; i++) {
		int value = random();
		list[i] = (random() % 2 == 0) ? value : -value;
	}
}

double *mc_gen_rand_double(size_t len)
{
	double *list = malloc(len * sizeof(double));

	if (list) {
		mc_init_rand_double(len, list);
	}

	return list;
}

void mc_init_rand_double(size_t len, double list[len])
{
        for (size_t i = 0; i < len; i++) {
                int value = random();
                value = (random() % 2 == 0) ? value : -value;
                list[i] = value / DBL_MAX;
        }
}
