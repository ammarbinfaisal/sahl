#ifndef DEBUG_H

#define DEBUG_H

#include "opcodes.h"
#include <stdint.h>

int print_opcode(uint8_t *code, int i);
void dissassemble(uint8_t *code, int length);

#endif
