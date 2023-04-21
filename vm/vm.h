#ifndef VM_H

#define VM_H

#include "common.h"
#include "func.h"
#include "obj.h"

#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>

VM *new_vm(uint8_t *code, int code_length);
VM *coro_vm(VM *curr, int start_func);
void free_vm(VM *vm);

#endif
