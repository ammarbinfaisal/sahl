#include "read.h"
#include <stdlib.h>
#include <string.h>

uint32_t read_u32(uint8_t *code, int idx) {
    union {
        uint8_t *restrict u8;
        uint32_t *restrict u32;
    } conv = {code + idx};
    return *conv.u32;
}

uint64_t read_u64(uint8_t *code, int idx) {
    union {
        uint8_t *restrict u8;
        uint32_t *restrict u64;
    } conv = {code + idx};
    return *conv.u64;
}

double read_double(uint8_t *code, int idx) {
    union {
        uint8_t *restrict u8;
        double *restrict d;
    } conv = {code + idx};
    return *conv.d;
}

char *read_string(uint8_t *code, int idx, int len) {
    char *str = malloc(len + 1);
    memcpy(str, code + idx, len);
    str[len] = '\0';
    return str;
}
