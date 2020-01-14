#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "mc_sort.h"

int main(int argc, char *argv[argc + 1])
{
	size_t const list_len = 6;
	int list[list_len] = { 4, 6, 1, 5, 3, 7 };
	size_t size = sizeof(int);

	assert(!is_sorted(list_len, list, size, int_compare));
	merge_sort(list_len, list, size, int_compare);
	assert(is_sorted(list_len, list, size, int_compare));

	double double_list[list_len] = { 4, 6, 1, 5, 3, 7 };
	size = sizeof(double);

	assert(!is_sorted(list_len, double_list, size, double_compare));
	merge_sort(list_len, double_list, size, double_compare);
	assert(is_sorted(list_len, double_list, size, double_compare));

	return EXIT_SUCCESS;
}
