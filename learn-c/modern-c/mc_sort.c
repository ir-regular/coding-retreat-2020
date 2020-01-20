#include <stdlib.h>
#include <string.h>
#include "mc_sort.h"

int mc_int_compare(const void *a, const void *b)
{
	int const *int_a = a;
	int const *int_b = b;

	return *int_a > *int_b ? 1 : (*int_a == *int_b ? 0 : -1);
}

int mc_double_compare(const void *a, const void *b)
{
	double const *double_a = a;
	double const *double_b = b;

	return *double_a > *double_b ? 1 : (*double_a == *double_b ? 0 : -1);
}

bool mc_is_sorted(size_t len, void *list, size_t size,
		  int (*mc_compare)(const void *a, const void *b))
{
	void *current = list;
	for (size_t i = 0; i < len - 1; i++) {
		if (mc_compare(current, current + size) == 1) {
			return false;
		}
		current += size;
	}
	return true;
}

void merge_sort2(size_t len, void *list, void *buffer, void *sorted_list,
		 size_t size, int (*mc_compare)(const void *a, const void *b));

void merge(size_t list_len, void *list, size_t buffer_len, void *buffer,
	   void *result, size_t size,
	   int (*mc_compare)(const void *a, const void *b));

void mc_merge_sort(size_t len, void *list, size_t size,
		   int (*mc_compare)(const void *a, const void *b))
{
	size_t buf_len = len / 2;
	void *buffer = malloc(len * size);

	if (buffer == 0) {
		exit(EXIT_FAILURE);
	}

	memset(buffer, 0, buf_len * size);

	merge_sort2(len, list, buffer, list, size, mc_compare);

	free(buffer);
}

void merge_sort2(size_t len, void *list, void *buffer, void *sorted_list,
		 size_t size, int (*mc_compare)(const void *a, const void *b))
{
	if (len < 2) {
		return;
	}

	// NOTE: +1 means this division will round up
	int half_len = (len + 1) / 2;
	int buffer_len = len - half_len;

	merge_sort2(half_len, list, buffer, list, size, mc_compare);

	// NOTE: list + half_len moves the pointer to the right
	if (buffer_len > 1) {
		merge_sort2(buffer_len, list + half_len * size, buffer, buffer,
			    size, mc_compare);
	} else {
		memmove(buffer, list + half_len * size, size);
	}

	merge(half_len, list, buffer_len, buffer, sorted_list, size,
	      mc_compare);
}

void merge(size_t list_len, void *list, size_t buffer_len, void *buffer,
	   void *result, size_t size,
	   int (*mc_compare)(const void *a, const void *b))
{
	void *list_i = list + list_len * size;
	void *buffer_i = buffer + buffer_len * size;
	void *result_i = result + (list_len + buffer_len) * size;

	while (result_i > result) {
		result_i -= size;

		if (list_i > list &&
		    (buffer_i == buffer ||
		     mc_compare(buffer_i - size, list_i - size) == -1)) {
			list_i -= size;
			memmove(result_i, list_i, size);
		} else {
			buffer_i -= size;
			memmove(result_i, buffer_i, size);
		}
	}
}

void quick_sort2(void *start, void *end, size_t size,
		 int (*mc_compare)(const void *a, const void *b),
		 void *pivot_buf, void *swap_buf);
void *partition(void *start, void *end, size_t size,
		int (*mc_compare)(const void *a, const void *b),
		void *pivot_buf, void *swap_buf);
void swap(void *a, void *b, void *buf, size_t size);

void mc_quick_sort(size_t len, void *list, size_t size,
		   int (*mc_compare)(const void *a, const void *b))
{
	void *pivot_buf = malloc(size);
	void *swap_buf = malloc(size);

	quick_sort2(list, ((unsigned char *)list) + (len - 1) * size, size,
		    mc_compare, pivot_buf, swap_buf);

	free(swap_buf);
	free(pivot_buf);
}

void quick_sort2(void *start, void *end, size_t size,
		 int (*mc_compare)(const void *a, const void *b),
		 void *pivot_buf, void *swap_buf)
{
	if (end <= start) {
		return;
	}

	unsigned char *pivot_p =
		partition(start, end, size, mc_compare, pivot_buf, swap_buf);

	if (pivot_p > (unsigned char *)start) {
		quick_sort2(start, pivot_p, size, mc_compare, pivot_buf,
			    swap_buf);
	}

	quick_sort2(pivot_p + size, end, size, mc_compare, pivot_buf, swap_buf);
}

void *partition(void *start, void *end, size_t size,
		int (*mc_compare)(const void *a, const void *b),
		void *pivot_buf, void *swap_buf)
{
	unsigned char *start_p = start;
	unsigned char *end_p = end;

	// you'd think you could eliminate size by combining the lines below,
	// but no: mid_index calculation uses integer division which rounds
	// down to 0
	size_t mid_index = (end_p - start_p) / (size * 2);
	unsigned char *pivot_source = start_p + mid_index * size;

	memmove(pivot_buf, pivot_source, size);

	unsigned char *less_than = start_p - size;
	unsigned char *more_than = end_p + size;

	while (true) {
		do {
			less_than += size;
		} while (mc_compare(less_than, pivot_buf) == -1);

		do {
			more_than -= size;
		} while (mc_compare(more_than, pivot_buf) == 1);

		if (more_than <= less_than) {
			break;
		}

		swap(less_than, more_than, swap_buf, size);
	}

	return more_than;
}

void swap(void *a, void *b, void *buf, size_t size)
{
	memmove(buf, a, size);
	memmove(a, b, size);
	memmove(b, buf, size);
}
