#include <stdio.h>

// int add(int a, int b, int c, int d, int e, int f, int g, int h) {
//     return a + b + c + d + e + f + g + h;
// }

struct Variable {
    int a;
    int b;
};

int main() {
    // int total = add(1, 2, 3, 4, 5, 6, 7, 8);

    // printf("Result: %i\n", total);

    struct Variable v = {1, 2};
    v.b = 9999;

    return 0;
}
