#ifndef MC_SORT_H
#define MC_SORT_H 1
#include <stdbool.h>

bool is_sorted(size_t len, void *list, size_t size,
	       int (*mc_compare)(const void *a, const void *b));

void merge_sort(size_t len, void *list, size_t size,
		int (*mc_compare)(const void *a, const void *b));

int int_compare(void const *a, void const *b);

int double_compare(void const *a, void const *b);

#endif