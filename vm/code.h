#ifndef CODE_H

#define CODE_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct Code {
    uint32_t start_ip;
    uint8_t *bytes;
    long length;
};

typedef struct Code Code;

Code *read_bytecode(const char *filename);

#endif
