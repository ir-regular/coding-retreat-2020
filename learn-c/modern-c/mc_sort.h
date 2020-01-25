#ifndef MC_SORT_H
#define MC_SORT_H 1
#include <stdbool.h>

bool mc_is_sorted(size_t len, void *list, size_t size,
		  int (*mc_compare)(const void *a, const void *b));

void mc_merge_sort(size_t len, void *list, size_t size,
		   int (*mc_compare)(const void *a, const void *b));

void mc_quick_sort(size_t len, void *list, size_t size,
		   int (*mc_compare)(const void *a, const void *b));

int mc_int_compare(const void *a, const void *b);

int mc_double_compare(void const *a, const void *b);

void mc_merge(size_t list_len, void *list, size_t buffer_len, void *buffer,
	      void *result, size_t size,
	      int (*mc_compare)(const void *a, const void *b));

#endif