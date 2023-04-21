#ifndef VM_H

#define VM_H

#include "common.h"
#include "func.h"
#include "obj.h"

#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>

#define APPEND_THREAD(thread)                                                  \
    vm->thread_count++;                                                        \
    vm->threads = realloc(vm->threads, sizeof(pthread_t) * vm->thread_count);  \
    vm->threads[vm->thread_count - 1] = thread;

VM *new_vm(uint8_t *code, int code_length);
VM *coro_vm(VM *curr, int start_func);
void free_vm(VM *vm);

#endif
