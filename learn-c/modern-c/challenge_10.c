#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "mc_gen.h"
#include "mc_sort.h"

double clock_t_to_ms(clock_t t);
double merge_sort_avg_time(const size_t test_case_size, int *test_case_buf,
			   const size_t sample_count);
double quick_sort_avg_time(const size_t test_case_size, int *test_case_buf,
			   const size_t sample_count);

int main(int argc, char *argv[argc + 1])
{
	printf("Comparing efficiency of homebrewed merge sort and quicksort\n");

	if (argc == 1) {
		printf("Requires at least one argument!\n");
		printf("Argument: count of elements to sort (positive int)\n");
		printf("Suggested values: 10000 50000 100000\n");
		return EXIT_FAILURE;
	}

	const size_t sample_count = 100;

	size_t *test_case_size = malloc(argc * sizeof(size_t));

	for (size_t i = 1; i < argc; i++) {
		test_case_size[i - 1] = strtoul(argv[i], 0, 0);
	}

	printf("%10s%13s%13s\n", "list size", "merge sort", "quick sort");

	for (size_t i = 0; i < argc - 1; i++) {
		int *test_case = malloc(test_case_size[i] * sizeof(int));

		double quick_sort_time = quick_sort_avg_time(
			test_case_size[i], test_case, sample_count);
		double merge_sort_time = merge_sort_avg_time(
			test_case_size[i], test_case, sample_count);

		printf("%10zu%10.2f ms%10.2f ms\n", test_case_size[i],
		       merge_sort_time, quick_sort_time);

		free(test_case);
	}

	free(test_case_size);

	return EXIT_SUCCESS;
}

double clock_t_to_ms(clock_t t)
{
	return t * 1000 / CLOCKS_PER_SEC;
}

double merge_sort_avg_time(const size_t test_case_size, int *test_case_buf,
			   const size_t sample_count)
{
	const size_t size = sizeof(int);
	double total_sort_time = 0;

	clock_t start = 0;
	clock_t end = 0;

	for (size_t sample = 0; sample < sample_count; sample++) {
		mc_init_rand_int(test_case_size, test_case_buf);

		start = clock();
		mc_merge_sort(test_case_size, test_case_buf, size,
			      mc_int_compare);
		end = clock();

		total_sort_time += clock_t_to_ms(end - start);
	}

	return (total_sort_time / sample_count);
}

double quick_sort_avg_time(const size_t test_case_size, int *test_case_buf,
			   const size_t sample_count)
{
	const size_t size = sizeof(int);
	double total_sort_time = 0;

	clock_t start = 0;
	clock_t end = 0;

	for (size_t sample = 0; sample < sample_count; sample++) {
		mc_init_rand_int(test_case_size, test_case_buf);

		start = clock();
		mc_quick_sort(test_case_size, test_case_buf, size,
			      mc_int_compare);
		end = clock();

		total_sort_time += clock_t_to_ms(end - start);
	}

	return (total_sort_time / sample_count);
}