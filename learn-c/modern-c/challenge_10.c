#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "mc_gen.h"
#include "mc_sort.h"

double clock_t_to_ms(clock_t t)
{
	return t * 1000 / CLOCKS_PER_SEC;
}

int main(int argc, char *argv[argc + 1])
{
	printf("Comparing efficiency of homebrewed merge sort and quicksort\n");

	if (argc == 1) {
		printf("Requires at least one argument!\n");
		printf("Argument: count of elements to sort (positive int)\n");
		printf("Suggested values: 10000 50000 100000\n");
		return EXIT_FAILURE;
	}

	size_t const size = sizeof(int);
	size_t *test_case_size = malloc(argc * sizeof(size_t));

	for (size_t i = 1; i < argc; i++) {
		test_case_size[i - 1] = strtoul(argv[i], 0, 0);
	}

	printf("%10s%13s%13s\n", "list size", "merge sort", "quick sort");

	for (size_t i = 0; i < argc - 1; i++) {
		int *test_case = mc_gen_rand_int(test_case_size[i]);

		clock_t start = clock();
		mc_merge_sort(test_case_size[i], test_case, size,
			      mc_int_compare);
		clock_t end = clock();

		double merge_sort_time = clock_t_to_ms(end - start);

		mc_init_rand_int(test_case_size[i], test_case);

		start = clock();
		mc_quick_sort(test_case_size[i], test_case, size,
			      mc_int_compare);
		end = clock();

		double quick_sort_time = clock_t_to_ms(end - start);

		printf("%10zu%10.2f ms%10.2f ms\n", test_case_size[i],
		       merge_sort_time, quick_sort_time);

		free(test_case);
	}

	free(test_case_size);

	return EXIT_SUCCESS;
}
