#ifndef GC_H

#define GC_H

#include "common.h"
#include "vm.h"

#define GC_HEAP_GROW_FACTOR 1.4


void cheney_collect(CheneyState *cs, VM *vm);
Obj *cheney_allocate(VM *vm, int count);

#endif