#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include "mc_gen.h"
#include "mc_sort.h"

bool is_sorted(size_t list_len, int list[list_len]);
void quick_sort(size_t len, int list[len]);
void quick_sort2(size_t start, size_t end, void *list);
void *partition(void *start, void *end, size_t size,
        int (*compare)(void const *a, void const *b));
void swap(void *a, void *b, void *buf, size_t size);

void test_int_sort(size_t const list_len);
void test_swap(void);
void test_partition(void);

int main(int argc, char *argv[argc + 1])
{
	int const list_len = 6;

        test_swap();
        test_partition();
        test_int_sort(list_len);

	return EXIT_SUCCESS;
}

void test_swap(void)
{
        size_t const size = sizeof(int);
        int list[3] = {1, 2, 3};
        void *buf = malloc(size);

        swap((void *)list, (void *)list + 2 * size, buf, size);

        free(buf);

        assert(list[0] == 3);
        assert(list[1] == 2);
        assert(list[2] == 1);
}

void test_partition(void)
{
        size_t const size = sizeof(int);
        void *pivot = 0;

        int list2[2] = { 4, 2 };
        pivot = partition(list2, list2 + 1, size, mc_int_compare);
        assert(list2 == pivot);
        assert(list2[0] == 2 && list2[1] == 4);

        int list3[3] = { 4, 3, 1};
        pivot = partition(list3, list3 + 2, size, mc_int_compare);
        assert(list3 + 1 == pivot); // this fails
        assert(list3[0] == 1 && list3[1] == 3 && list3[2] == 4);

        int list4[4] = { 1, 3, 2, 0 };
        pivot = partition(list4, list4 + 3, size, mc_int_compare);
        assert(list4 + 2 == pivot);
        assert(list4[0] == 1 && list4[1] == 0 && list4[2] == 2 && list4[3] == 3);
}

void test_int_sort(size_t const list_len)
{
        int *list = mc_gen_rand_int(list_len);

	assert(!is_sorted(list_len, list));
	quick_sort(list_len, list);
	assert(is_sorted(list_len, list));

	free(list);
}

bool is_sorted(size_t list_len, int list[list_len])
{
	for (size_t i = 0; i < list_len - 1; i++) {
		if (list[i] > list[i + 1]) {
			return false;
		}
	}
	return true;
}

void quick_sort(size_t len, int *list)
{
        quick_sort2(0, len - 1, list);
}

void quick_sort2(size_t start, size_t end, void *list)
{
	if (end <= start) {
		return;
	}

        size_t const size = sizeof(int);

        unsigned char *list_p = list;
	unsigned char *pivot_p = partition(list_p + start * size, list_p + end * size, size, mc_int_compare);

	ptrdiff_t pivot_i = (pivot_p - list_p) / size;

	if (pivot_i > 0) {
		quick_sort2(start, pivot_i, list);
	}

	quick_sort2(pivot_i + 1, end, list);
}

void *partition(void *start, void *end, size_t size,
        int (*compare)(void const *a, void const *b))
{
        unsigned char *start_p = start;
        unsigned char *end_p = end;

        // you'd think you could eliminate size by combining the lines below,
        // but no: mid_index calculation uses integer division which rounds
        // down to 0
        size_t mid_index = (end_p - start_p) / (size * 2);
        unsigned char *pivot_source = start_p + mid_index * size;

        unsigned char *pivot = malloc(size);
        memmove(pivot, pivot_source, size);

	unsigned char *less_than = start_p - size; // can be -1 when just starting out
	unsigned char *more_than = end_p + size;
	void *swap_buf = malloc(size);

	while (true) {
		do {
			less_than += size;
		} while (compare(less_than, pivot) == -1);

		do {
			more_than -= size;
		} while (compare(more_than, pivot) == 1);

		if (more_than <= less_than) {
			break;
		}

                swap(less_than, more_than, swap_buf, size);
	}

	free(swap_buf);
	free(pivot);

	return more_than;
}

void swap(void *a, void *b, void *buf, size_t size)
{
	memmove(buf, a, size);
	memmove(a, b, size);
	memmove(b, buf, size);
}
