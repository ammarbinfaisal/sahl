#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void iprint(int64_t i) { printf("%ld\n", i); }

void fprint(double f) { printf("%f\n", f); }

void cprint(char c) { printf("%c\n", c); }

void bprint(int b) { printf("%s\n", b ? "true" : "false"); }

double ifadd(int64_t a, double b) { return a + b; }

double ffadd(double a, double b) { return a + b; }

double ifsub(int64_t a, double b) { return a - b; }

double ffsub(double a, double b) { return a - b; }

double ifmul(int64_t a, double b) { return a * b; }

double ffmul(double a, double b) { return a * b; }

int64_t iidiv(int64_t a, int64_t b) { return a / b; }

double ifdiv(int64_t a, double b) { return a / b; }

double fidiv(double a, int64_t b) { return a / b; }

double ffdiv(double a, double b) { return a / b; }

int ifcmp(int64_t a, double b) { return a < (int64_t)b ? -1 : a > b ? 1 : 0; }

int ffcmp(double a, double b) { return a < b ? -1 : a > b ? 1 : 0; }

void exit_with(int32_t code) { exit(code); }
