#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

void print_vector(size_t const len, double vector[len]) {
    printf("[ ");
    for (size_t i = 0; i < len; i++) {
        printf("%.2f ", vector[i]);
    }
    printf("]");
}

void print_matrix(size_t const x, size_t const y, double matrix[x][y]) {
    printf("[\n");

    for (size_t i = 0; i < x; i++) {
        printf("\t");
        print_vector(y, matrix[i]);
        printf(",\n");
    }

    printf("]");
}

double dot_product(size_t const len, double vector_a[len], double vector_b[len]) {
    double result = 0;
    for (size_t i = 0; i < len; i++) {
        result += vector_a[i] * vector_b[i];
    }
    return result;
}

void matrix_mult(size_t const x, size_t const y, size_t const shared, double matrix_a[x][shared], double matrix_b[shared][y], double result[x][y]) {
    for (size_t j = 0; j < y; j++) {
        double column_b[shared];

        for (size_t i = 0; i < shared; i++) {
            column_b[i] = matrix_b[i][j];
        }

        for (size_t i = 0; i < x; i++) {
            result[i][j] = dot_product(shared, matrix_a[i], column_b);
        }
    }
}

int main(int argc, char const* argv[argc+1]) {
    const size_t len = 3;

    double vector_a[len] = {1,  3, -5};
    double vector_b[len] = {4, -2, -1};

    printf("Vector a:\n");
    print_vector(len, vector_a);
    printf("\nVector b:\n");
    print_vector(len, vector_b);

    double dot_result = dot_product(len, vector_a, vector_b);

    printf("\nDot product: %.2f\n", dot_result);

    assert(3 == dot_result);

    double matrix_a[2][3] = {{1, 2, 3}, {4, 5, 6}};
    double matrix_b[3][2] = {{7, 8}, {9, 10}, {11, 12}};
    double result[2][2];

    printf("Matrix a:\n");
    print_matrix(2, 3, matrix_a);
    printf("\nMatrix b:\n");
    print_matrix(3, 2, matrix_b);

    matrix_mult(2, 2, 3, matrix_a, matrix_b, result);

    printf("\nResult:\n");
    print_matrix(2, 2, result);
    printf("\n");

    assert(58 == result[0][0]);
    assert(64 == result[0][1]);
    assert(139 == result[1][0]);
    assert(154 == result[1][1]);

    return EXIT_SUCCESS;
}
