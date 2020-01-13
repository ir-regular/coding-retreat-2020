#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

void quick_sort(size_t len, int list[len]);
void merge_sort(size_t list_len, int list[list_len]);
void init_random(size_t len, int list[len]);
double clock_t_to_ns(clock_t t);

int main(int argc, char *argv[argc + 1])
{
	size_t const test_case_count = 3;
	size_t const test_case_size[test_case_count] = { 10, 1000, 10000 };

	for (size_t i = 0; i < test_case_count; i++) {
		int test_case[test_case_size[i]];

		init_random(test_case_size[i], test_case);

		clock_t start = clock();
		merge_sort(test_case_size[i], test_case);
		clock_t end = clock();

		double merge_sort_time = clock_t_to_ns(end - start);

		init_random(test_case_size[i], test_case);

		start = clock();
		quick_sort(test_case_size[i], test_case);
		end = clock();

		double quick_sort_time = clock_t_to_ns(end - start);

		printf("List size: %zu\nmerge sort: %.1f ns\nquick sort: %.1f ns\n",
		       test_case_size[i], merge_sort_time, quick_sort_time);
	}
}

double clock_t_to_ns(clock_t t)
{
	return t * 1000000000 / CLOCKS_PER_SEC
}

void init_random(size_t len, int list[len])
{
	for (size_t i = 0; i < len; i++) {
		list[i] = rand() % INT_MAX;
	}
}

// These functions are copied from challenge_1_1.c and challenge_1_2.c - this is
// for practice, I have limited time for cleanups

void quick_sort2(size_t start, size_t end, int *list);
size_t partition(size_t start, size_t end, int *list);
void swap(size_t i, size_t j, int *list);
void merge_sort2(size_t list_len, int list[list_len], int buffer[list_len / 2],
		 int sorted_list[list_len]);
void merge(size_t list_len, int list[list_len], size_t buffer_len,
	   int buffer[buffer_len], int result[list_len + buffer_len]);

void quick_sort(size_t len, int list[len])
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
		quick_sort2(start, pivot_i - 1, list);
	}

	quick_sort2(pivot_i + 1, end, list);
}

size_t partition(size_t start, size_t end, int *list)
{
	int pivot = list[start + (end - start) / 2];
	int less_than = start - 1;
	int more_than = end + 1;

	while (true) {
		do {
			less_than++;
		} while (list[less_than] < pivot);

		do {
			more_than--;
		} while (list[more_than] > pivot);

		if (more_than <= less_than) {
			return more_than;
		}

		swap(less_than, more_than, list);
	}
}

void swap(size_t i, size_t j, int *list)
{
	size_t temp = list[i];
	list[i] = list[j];
	list[j] = temp;
}

void merge_sort(size_t list_len, int list[list_len])
{
	size_t buf_len = list_len / 2;
	int buffer[buf_len];
	memset(buffer, 0, buf_len * sizeof(int));

	merge_sort2(list_len, list, buffer, list);
}

void merge_sort2(size_t list_len, int list[list_len], int buffer[list_len / 2],
		 int sorted_list[list_len])
{
	if (list_len < 2) {
		return;
	}

	// NOTE: +1 means this division will round up
	int half_len = (list_len + 1) / 2;
	int buffer_len = list_len - half_len;

	merge_sort2(half_len, list, buffer, list);

	// NOTE: list + half_len moves the pointer to the right
	if (buffer_len > 1) {
		merge_sort2(buffer_len, list + half_len, buffer, buffer);
	} else {
		buffer[0] = list[half_len];
	}

	merge(half_len, list, buffer_len, buffer, sorted_list);
}

void merge(size_t list_len, int list[list_len], size_t buffer_len,
	   int buffer[buffer_len], int result[list_len + buffer_len])
{
	size_t list_i = list_len;
	size_t buffer_i = buffer_len;
	size_t i = list_len + buffer_len;

	while (i > 0) {
		if (list_i > 0 && (buffer_i == 0 ||
				   buffer[buffer_i - 1] < list[list_i - 1])) {
			result[i - 1] = list[list_i - 1];
			list_i--;
		} else {
			result[i - 1] = buffer[buffer_i - 1];
			buffer_i--;
		}

		i--;
	}
}
