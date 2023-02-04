#include <stdio.h>

void print_str(char *str) { puts(str); }

void print_int(long long i) { printf("%lld\n", i); }

void print_char(char c) { printf("%c\n", c); }

void print_bool(int b) { printf("%s\n", b ? "true" : "false"); }
