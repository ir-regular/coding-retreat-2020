#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "mc_sort.h"
#include "mc_gen.h"

void test_int_sort(size_t list_len);
void test_double_sort(size_t list_len);

int main(int argc, char *argv[argc + 1])
{
	size_t const list_len = 6;

	test_int_sort(list_len);
	test_double_sort(list_len);

	return EXIT_SUCCESS;
}

void test_int_sort(size_t list_len)
{
	int *list = mc_gen_rand_int(list_len);
	size_t const size = sizeof(int);

	assert(!is_sorted(list_len, list, size, int_compare));
	merge_sort(list_len, list, size, int_compare);
	assert(is_sorted(list_len, list, size, int_compare));

	free(list);
}

void test_double_sort(size_t list_len)
{
	double *list = mc_gen_rand_double(list_len);
	size_t const size = sizeof(double);

	assert(!is_sorted(list_len, list, size, double_compare));
	merge_sort(list_len, list, size, double_compare);
	assert(is_sorted(list_len, list, size, double_compare));

	free(list);
}
