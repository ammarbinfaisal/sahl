#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

void iprint(int64_t i) {
    printf("%ld", i);
}

void fprint(double f) {
    printf("%lf", f);
}

void cprint(char c) {
    printf("%c", c);
}

void bprint(int b) {
    printf("%s", b ? "true" : "false");
}

void sprint(char *s) {
    printf("%s", s);
}

