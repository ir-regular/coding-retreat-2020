#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <math.h>
#include "mc_gen.h"
#include "mc_sort.h"

typedef struct sort_sample sort_sample;
struct sort_sample {
	size_t len;
	void *sample;
	size_t size;
	int (*compare)(const void *, const void *);
};

void *msort_fun(void *ptr);
void *qsort_fun(void *ptr);

void reduce(size_t thread_count, pthread_t threads[thread_count],
	    sort_sample thread_params[thread_count], size_t sample_len,
	    void *sample, const size_t size,
	    int (*compare)(const void *, const void *));

void map(size_t thread_count, pthread_t threads[thread_count],
	 sort_sample thread_params[thread_count],
	 void *(*start_routine)(void *), size_t sample_len, void *sample,
	 const size_t size, int (*compare)(const void *, const void *));

void sort_unthreaded(const size_t sample_len, void *sample, const size_t size,
		     int (*compare)(const void *, const void *));

void sort_threaded(const size_t half_thread_count, const size_t sample_len,
		   void *sample, const size_t size,
		   int (*compare)(const void *, const void *));

int main(int argc, char *argv[argc + 1])
{
	if (argc != 2) {
		printf("Use: %s [n] where 2*n is the number of threads that will run\n",
		       argv[0]);
		return EXIT_FAILURE;
	}

	size_t half_thread_count = strtoul(argv[1], 0, 0);

	int (*compare)(const void *, const void *) = mc_int_compare;
	const size_t size = sizeof(int);
	const size_t sample_len = 6;
	int *sample = malloc(sample_len * size);
	mc_init_rand_int(sample_len, sample);

	assert(!mc_is_sorted(sample_len, sample, size, compare));

	if (half_thread_count == 0) {
		sort_unthreaded(sample_len, sample, size, compare);
	} else {
		sort_threaded(half_thread_count, sample_len, sample, size,
			      compare);
	}

	assert(mc_is_sorted(sample_len, sample, size, compare));

	free(sample);

	return EXIT_SUCCESS;
}

void sort_unthreaded(const size_t sample_len, void *sample, const size_t size,
		     int (*compare)(const void *, const void *))
{
	mc_merge_sort(sample_len, sample, size, compare);
}

void sort_threaded(const size_t half_thread_count, const size_t sample_len,
		   void *sample, const size_t size,
		   int (*compare)(const void *, const void *))
{
	size_t thread_count = half_thread_count * 2;

	pthread_t *threads = malloc(thread_count * sizeof(pthread_t));
	sort_sample *thread_params = malloc(thread_count * sizeof(sort_sample));

	map(half_thread_count, threads, thread_params, msort_fun,
	    sample_len / 2, sample, size, compare);
	map(half_thread_count, &threads[half_thread_count],
	    &thread_params[half_thread_count], qsort_fun, sample_len / 2,
	    ((unsigned char *)sample + size * half_thread_count), size,
	    compare);

	reduce(thread_count, threads, thread_params, sample_len, sample, size,
	       compare);

	free(threads);
	free(thread_params);
}

void init_sort_sample(sort_sample *thread_params, size_t *items_remaining,
		      const size_t batch_size, void **sample, const size_t size,
		      int (*compare)(const void *, const void *))
{
	size_t current_batch =
		*items_remaining >= batch_size ? batch_size : *items_remaining;

	thread_params->size = size;
	thread_params->compare = compare;
	thread_params->len = current_batch;
	thread_params->sample = *sample;

	*items_remaining = *items_remaining - current_batch;
	*sample = ((unsigned char *)*sample) + current_batch * size;
}

void map(size_t thread_count, pthread_t threads[thread_count],
	 sort_sample thread_params[thread_count],
	 void *(*start_routine)(void *), size_t sample_len, void *sample,
	 const size_t size, int (*compare)(const void *, const void *))
{
	// ceil of sample_len / thread_count
	size_t batch_size = 1 + ((sample_len - 1) / thread_count);

	for (size_t i = 0; i < thread_count; i++) {
		init_sort_sample(&thread_params[i], &sample_len, batch_size,
				 &sample, size, compare);

		pthread_create(&threads[i], 0, start_routine,
			       &thread_params[i]);
	}
}

void reduce(size_t thread_count, pthread_t threads[thread_count],
	    sort_sample thread_params[thread_count], size_t sample_len,
	    void *sample, const size_t size,
	    int (*compare)(const void *, const void *))
{
	for (size_t i = 0; i < thread_count; i++) {
		pthread_join(threads[i], 0);
	}

	// ceil of sample_len / thread_count
	size_t batch_len = 1 + ((sample_len - 1) / thread_count);
	size_t batch_size = batch_len * size;

	unsigned char *batch_1 = sample;
	unsigned char *batch_2 = batch_1 + batch_size;

	while (batch_len < sample_len) {
		batch_len = batch_len * 2;

		for (size_t i = batch_len; i < sample_len; i += batch_len) {
			mc_merge(batch_size, batch_1, batch_size, batch_2,
				 batch_1, size, compare);

			batch_1 = batch_2 + batch_size;
			batch_2 = batch_1 + batch_size;
		}

		batch_size = batch_size * 2;
	}

	if (batch_len > sample_len) {
		batch_len /= 2;
		batch_1 = sample;
		batch_2 = sample + batch_size;

		mc_merge(batch_size, batch_1, (sample_len - batch_len) * size,
			 batch_2, sample, size, compare);
	}
}

static pthread_key_t key;
static pthread_once_t key_once = PTHREAD_ONCE_INIT;

static void make_key()
{
	(void)pthread_key_create(&key, NULL);
}

void *msort_fun(void *ptr)
{
	struct sort_sample *restrict state = ptr;

	mc_merge_sort(state->len, state->sample, state->size, state->compare);

	return state;
}

void *qsort_fun(void *ptr)
{
	struct sort_sample *restrict state = ptr;

	mc_quick_sort(state->len, state->sample, state->size, state->compare);

	return state;
}
