#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

bool is_sorted(size_t list_len, int list[list_len]);
void merge_sort(size_t list_len, int list[list_len], int buffer[list_len/2], int sorted_list[list_len]);
void merge(size_t list_len, int list[list_len], size_t helper_len, int helper[helper_len], int result[list_len]);
void print_list(size_t len, int list[len]);

int main(int argc, char* argv[argc+1]) {
    int list_len = 6;
    int list[6] = {4, 6, 1, 5, 3, 7};
    int buffer[3];

    printf("Should be false: %d\n", is_sorted(list_len, list));

    merge_sort(list_len, list, buffer, list);

    printf("Should be true: %d\n", is_sorted(list_len, list));

    return EXIT_SUCCESS;
}

bool is_sorted(size_t list_len, int list[list_len]) {
    for (size_t i = 0; i < list_len - 1; i++) {
        if (list[i] > list[i+1]) {
            return false;
        }
    }
    return true;
}

void merge_sort(size_t list_len, int list[list_len], int buffer[list_len/2], int sorted_list[list_len]) {
    if (list_len < 2) {
        return;
    }

    printf("Sorting: ");
    print_list(list_len, list);

    // NOTE: +1 means this division will round up
    int half_len = (list_len + 1) / 2;
    int buffer_len = list_len - half_len;

    merge_sort(half_len, list, buffer, list);

    printf("First half sorted: ");
    print_list(half_len, sorted_list);

    // NOTE: list + half_len moves the pointer to the right
    if (buffer_len > 1) {
        merge_sort(buffer_len, list + half_len, buffer, buffer);
    } else {
        buffer[0] = list[half_len];
    }

    printf("Second half sorted: ");
    print_list(buffer_len, buffer);

    merge(half_len, list, buffer_len, buffer, sorted_list);
}

void merge(size_t list_len, int list[list_len], size_t buffer_len, int buffer[buffer_len], int result[list_len+buffer_len]) {
    size_t list_i = list_len;
    size_t buffer_i = buffer_len;
    size_t i = list_len + buffer_len;

    while (i > 0) {
        printf("result %zu, list %zu, buffer %zu\n", i, list_i, buffer_i);

        if (list_i > 0 && (buffer_i == 0 || buffer[buffer_i-1] < list[list_i-1])) {
            result[i-1] = list[list_i-1];
            list_i--;
        } else {
            result[i-1] = buffer[buffer_i-1];
            buffer_i--;
        }

        i--;
    }

    printf("Merged: ");
    print_list(list_len+buffer_len, result);
}

void print_list(size_t len, int list[len]) {
    printf("[");
    for (size_t i = 0; i < len; i++) {
        printf(" %d", list[i]);
    }
    printf(" ]\n");
}