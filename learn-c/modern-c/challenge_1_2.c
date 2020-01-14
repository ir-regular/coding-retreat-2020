#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mc_gen.h"

bool is_sorted(size_t list_len, int list[list_len]);
void quick_sort(size_t len, int list[len]);
void quick_sort2(size_t start, size_t end, int *list);
size_t partition(size_t start, size_t end, int *list);
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
        size_t pivot = 0;

        int list2[2] = { 4, 2 };
        pivot = partition(0, 1, list2);
        assert(0 == pivot);
        assert(list2[0] == 2 && list2[1] == 4);

        int list3[3] = { 4, 3, 1};
        pivot = partition(0, 2, list3);
        assert(1 == pivot);
        assert(list3[0] == 1 && list3[1] == 3 && list3[2] == 4);

        int list4[4] = { 1, 3, 2, 0 };
        pivot = partition(0, 3, list4);
        assert(2 == pivot);
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

void quick_sort2(size_t start, size_t end, int *list)
{
	if (end <= start) {
		return;
	}

	size_t pivot_i = partition(start, end, list);

	if (pivot_i > 0) {
		quick_sort2(start, pivot_i, list);
	}

	quick_sort2(pivot_i + 1, end, list);
}

size_t partition(size_t start, size_t end, int *list)
{
        size_t const size = sizeof(int);
	int pivot = list[(start + end) / 2];
	int less_than = start - 1; // can be -1 when just starting out
	int more_than = end + 1;
	void *swap_buf = malloc(size);

	while (true) {
		do {
			less_than++;
		} while (list[less_than] < pivot);

		do {
			more_than--;
		} while (list[more_than] > pivot);

		if (more_than <= less_than) {
			break;
		}

		swap((void *)list + less_than * size, (void *)list + more_than * size, swap_buf, size);
	}

	free(swap_buf);

	return more_than;
}

void swap(void *a, void *b, void *buf, size_t size)
{
	memmove(buf, a, size);
	memmove(a, b, size);
	memmove(b, buf, size);
}
