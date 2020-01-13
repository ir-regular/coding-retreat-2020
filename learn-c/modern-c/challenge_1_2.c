#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

bool is_sorted(size_t list_len, int list[list_len]);
void quick_sort(size_t start, size_t end, int *list);
size_t partition(size_t start, size_t end, int *list);
void swap(size_t i, size_t j, int *list);
void print_list(size_t len, int list[len]);

int main(int argc, char *argv[argc + 1])
{
	int list_len = 6;
	int list[6] = { 4, 6, 1, 5, 3, 7 };

	printf("List is unsorted: ");
	print_list(list_len, list);

	printf("Sorting via quicksort\n");
	quick_sort(0, list_len - 1, list);

	if (is_sorted(list_len, list)) {
		printf("List is sorted\n");
	} else {
		printf("Bug in sorting procedure!\n");
		print_list(list_len, list);
	}

	return EXIT_SUCCESS;
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

void quick_sort(size_t start, size_t end, int *list)
{
	if (end <= start) {
		return;
	}

	size_t pivot_i = partition(start, end, list);

	if (pivot_i > 0) {
		quick_sort(start, pivot_i - 1, list);
	}

	quick_sort(pivot_i + 1, end, list);
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

void print_list(size_t len, int list[len])
{
	printf("[");
	for (size_t i = 0; i < len; i++) {
		printf(" %d", list[i]);
	}
	printf(" ]\n");
}