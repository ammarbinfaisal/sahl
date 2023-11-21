#include <math.h>
#include <stdlib.h>

double randf() {
    return (double)rand() / (double)RAND_MAX;
}

double pow(double x, double y) {
    return powf(x, y);
}

extern double log(double);

extern double exp(double);
