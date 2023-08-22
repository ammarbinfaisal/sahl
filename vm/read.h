#ifndef READ_H

#define READ_H

#include <stdint.h>

inline uint32_t read_u32(uint8_t *code, int idx) {
    union {
        uint8_t *restrict u8;
        uint32_t *restrict u32;
    } conv = {code + idx};
    return *conv.u32;
}

inline uint64_t read_u64(uint8_t *code, int idx) {
    union {
        uint8_t *restrict u8;
        uint64_t *restrict u64;
    } conv = {code + idx};
    return *conv.u64;
}

inline double read_double(uint8_t *code, int idx) {
    union {
        uint8_t *restrict u8;
        double *restrict d;
    } conv = {code + idx};
    return *conv.d;
}
char *read_string(uint8_t *code, int idx, int len);

#endif
