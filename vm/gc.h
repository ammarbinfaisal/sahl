#ifndef GC_H

#define GC_H

#include "common.h"
#include "vm.h"

#define GC_HEAP_GROW_FACTOR 1.4

void * checked_malloc(size_t size);
void cheney_collect(CheneyState *cs, VM *vm);
void *cheney_allocate(VM *vm, int count);

#endif