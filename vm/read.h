#ifndef READ_H

#define READ_H

#include <stdint.h>

uint32_t read_u32(uint8_t *code, int idx);
uint64_t read_u64(uint8_t *code, int idx);
double read_double(uint8_t *code, int idx);
char *read_string(uint8_t *code, int idx, int len);

#endif
