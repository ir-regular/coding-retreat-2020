#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

/**
 * For a given index, finds the root of its tree
 */
size_t Find(size_t len, size_t parent[len], size_t element) {
    while (parent[element] != SIZE_MAX) {
        element = parent[element];
    }

    return element;
}

/**
 * Changes all parent entries on a path to the root (including) to a specific value
 */
void FindReplace(size_t len, size_t parent[len], size_t element, size_t new_root) {
    while (parent[element] != SIZE_MAX) {
        size_t temp = parent[element];
        parent[element] = new_root;
        element = temp;
    }
    parent[element] = new_root;
}

/**
 * Changes all parent entries to the root that has been found
 */
size_t FindCompress(size_t len, size_t parent[len], size_t element) {
    size_t root = Find(len, parent, element);
    FindReplace(len, parent, element, root);
    parent[root] = SIZE_MAX; // re-rootify the root
    return root;
}

/**
 * For two given elements, combines their trees into one.
 */
void Union(size_t len, size_t parent[len], size_t element_a, size_t element_b) {
    size_t root = FindCompress(len, parent, element_a);
    FindReplace(len, parent, element_b, root);
}

int main(int argc, const char* argv[argc]) {
  size_t set_size = 10;
  size_t parent[10];

  for (size_t i = 0; i < set_size; i++) {
    parent[i] = SIZE_MAX;
  }

  assert(Find(set_size, parent, 3) == 3);

  Union(set_size, parent, 3, 4);
  Union(set_size, parent, 3, 5);
  Union(set_size, parent, 3, 8);

  Union(set_size, parent, 2, 1);
  Union(set_size, parent, 2, 9);
  Union(set_size, parent, 2, 6);

  assert(Find(set_size, parent, 3) == 3);
  assert(Find(set_size, parent, 8) == 3);

  assert(Find(set_size, parent, 1) == 2);

  assert(Find(set_size, parent, 7) == 7);

  return EXIT_SUCCESS;
}

