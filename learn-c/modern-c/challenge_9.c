#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LIST_END 0

void mark_primes(size_t len, bool nums[len]);
void mark_factors(unsigned n, size_t prime_len, bool primes[prime_len],
		  size_t num_len, size_t factors[num_len]);

int main(int argc, const char *argv[argc])
{
	if (argc == 1) {
		printf("You must provide an integer argument\n");
		return EXIT_FAILURE;
	}

	for (size_t arg = 1; arg < argc; arg++) {
		char *ptr = 0;
		unsigned long n = strtoul(argv[arg], &ptr, 10);

		unsigned prime_cnt = ceil(sqrt(n)) + 1;
		unsigned factor_cnt = ceil(log2(n));

		bool primes[prime_cnt];
		size_t factors[factor_cnt];

		mark_primes(prime_cnt, primes);
		mark_factors(n, prime_cnt, primes, factor_cnt, factors);

		printf("%lu: ", n);
		for (size_t i = 0; i < factor_cnt; i++) {
			if (factors[i] == LIST_END) {
				break;
			}
			printf("%zu ", factors[i]);
		}
		printf("\n");
	}

	return EXIT_SUCCESS;
}

void mark_primes(size_t len, bool nums[len])
{
	memset(nums, true, len * sizeof(bool));

	nums[0] = false;
	nums[1] = false;

	for (size_t i = 2; i < len; i++) {
		if (!nums[i]) {
			continue;
		}

		for (size_t j = 2; j * i < len; j++) {
			nums[j * i] = false;
		}
	}
}

void mark_factors(unsigned n, size_t prime_len, bool primes[prime_len],
		  size_t num_len, size_t factors[num_len])
{
	size_t end = 0;
	size_t factor = 2;
	factors[end] = LIST_END;

	while ((factor < prime_len) && (end < num_len)) {
		if (!primes[factor] || (n % factor != 0)) {
			// not a valid factor
			factor++;
			continue;
		}

		factors[end] = factor;
		end++;
		factors[end] = LIST_END;
		n = n / factor;
	}

	if (n > 1) {
		factors[end] = n;
		end++;
		factors[end] = LIST_END;
	}
}
