#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

bool bfs(size_t const node_count, bool graph[node_count][node_count],
	 size_t start, size_t needle)
{
	// has this node been searched?
	bool seen[node_count];
	// (defaults all elements to 0, which is false)
	memset(seen, false, node_count);

	// there can be max (node_count) nodes in the queue, if we do not re-queue
	// them
	size_t queue[node_count];

	size_t queue_start = 0;
	size_t queue_end = 1;
	queue[queue_start] = start;

	while (queue_start < node_count && queue_start != queue_end) {
		size_t node = queue[queue_start];

		if (node == needle) {
			return true;
		}

		seen[node] = true;

		// I could use a searching function, but this is easier to write
		for (size_t child = 0; child < node_count; child++) {
			if (graph[node][child] && !seen[child]) {
				queue[queue_end] = child;
				queue_end++;
			}
		}

		queue_start++;
	}

	return false;
}

/**
 * Note: not minimum spanning tree.
 */
void spanning_tree(size_t node_count, bool graph[node_count][node_count],
		   size_t parent[node_count])
{
	// has this node been searched?
	bool seen[node_count];
	memset(seen, false,
	       node_count); // (defaults all elements to 0, which is false)

	// there can be max (node_count) nodes in the queue, if we do not re-queue
	// them
	size_t queue[node_count];

	size_t queue_start = 0;
	size_t queue_end = 0;
	bool *root;

	while ((root = memchr(seen, 0, node_count))) {
		queue[queue_start] = (root - seen); // index of the next root
		parent[(root - seen)] = SIZE_MAX;
		queue_end++;

		while (queue_start < node_count && queue_start != queue_end) {
			size_t node = queue[queue_start];

			seen[node] = true;

			// I could use a searching function, but this is easier to write
			for (size_t child = 0; child < node_count; child++) {
				if (graph[node][child] && !seen[child]) {
					parent[child] = node;

					queue[queue_end] = child;
					queue_end++;
				}
			}

			queue_start++;
		}
	}
}

int main(int argc, char const *argv[argc + 1])
{
	size_t const nodes = 4;
	// note: assuming a directed graph here
	bool graph[nodes][nodes] = {
		{ false, true, false, false },
		{ false, false, true, false },
		{ false, false, false, false },
		{ false, false, false, false },
	};

	assert(bfs(nodes, graph, 0, 2)); // can go from 0 to 2
	assert(!bfs(nodes, graph, 0, 3)); // cannot go from 0 to 3

	size_t parents[nodes] = { 0 };

	spanning_tree(nodes, graph, parents);

	assert(SIZE_MAX == parents[0]);
	assert(0 == parents[1]);
	assert(1 == parents[2]);
	assert(SIZE_MAX == parents[3]);

	return EXIT_SUCCESS;
}