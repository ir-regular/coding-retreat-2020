#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "mc_gen.h"
#include "mc_sort.h"

void test_int_sort(size_t const list_len);
void test_double_sort(size_t const list_len);

int main(int argc, char *argv[argc + 1])
{
	int const list_len = 6;

	test_int_sort(list_len);
	test_double_sort(list_len);

	return EXIT_SUCCESS;
}

void test_int_sort(size_t list_len)
{
	size_t size = sizeof(int);
	int *list = malloc(list_len * size);

        if (list == 0) {
                exit(EXIT_FAILURE);
        }

	mc_init_rand_int(list_len, list);

	assert(!mc_is_sorted(list_len, list, size, mc_int_compare));
	mc_merge_sort(list_len, list, size, mc_int_compare);
	assert(mc_is_sorted(list_len, list, size, mc_int_compare));

	mc_init_rand_int(list_len, list);

	assert(!mc_is_sorted(list_len, list, size, mc_int_compare));
	mc_quick_sort(list_len, list, size, mc_int_compare);
	assert(mc_is_sorted(list_len, list, size, mc_int_compare));

	free(list);
}

void test_double_sort(size_t list_len)
{
	size_t size = sizeof(double);
	double *list = malloc(list_len * size);

	if (list == 0) {
		exit(EXIT_FAILURE);
	}

        mc_init_rand_double(list_len, list);

	assert(!mc_is_sorted(list_len, list, size, mc_double_compare));
	mc_merge_sort(list_len, list, size, mc_double_compare);
	assert(mc_is_sorted(list_len, list, size, mc_double_compare));

        mc_init_rand_double(list_len, list);

	assert(!mc_is_sorted(list_len, list, size, mc_double_compare));
	mc_quick_sort(list_len, list, size, mc_double_compare);
	assert(mc_is_sorted(list_len, list, size, mc_double_compare));

	free(list);
}
