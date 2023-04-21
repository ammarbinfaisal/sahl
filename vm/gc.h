#ifndef GC_H

#define GC_H

#include "common.h"
#include "vm.h"

#define GC_HEAP_GROW_FACTOR 1.4

void mark_obj(VM *vm, Obj *obj);
void mark_value(VM *vm, Value value);
static void blacken_object(VM *vm, Obj *obj);
static void trace_references(VM *vm);
static void sweep(VM *vm);
void mark_roots(VM *vm);
void collect_garbage(VM *vm);
void *allocate(VM *vm, size_t size);
void *reallocate(VM *vm, void *ptr, size_t oldSize, size_t newSize);

#endif