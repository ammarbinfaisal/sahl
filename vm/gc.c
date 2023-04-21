#include "gc.h"
#include <stdio.h>
#include <stdlib.h>

void mark_obj(VM *vm, Obj *obj) {
#ifdef DEBUG
    printf("marking %p\n", obj);
    print_value(OBJ_VAL(obj));
#endif

    if (obj->marked) return;
    obj->marked = true;

    if (vm->grayCapacity < vm->grayCount + 1) {
        vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
        vm->grayStack =
            (Obj **)realloc(vm->grayStack, sizeof(Obj *) * vm->grayCapacity);
    }

    vm->grayStack[vm->grayCount++] = obj;
}

void mark_value(VM *vm, Value value) {
    if (IS_OBJ(value)) {
        mark_obj(vm, AS_OBJ(value));
    }
}

static void blacken_object(VM *vm, Obj *obj) {
    switch (obj->type) {
    case OBJ_STRING: {
        break;
    }
    case OBJ_LIST: {
        for (int i = 0; i < obj->list.length; i++) {
            mark_value(vm, obj->list.items[i]);
        }
        break;
    }
    case OBJ_TUPLE: {
        for (int i = 0; i < obj->tuple.length; i++) {
            mark_value(vm, obj->tuple.items[i]);
        }
        break;
    }
    }
}

static void trace_references(VM *vm) {
    while (vm->grayCount > 0) {
        Obj *object = vm->grayStack[--vm->grayCount];
        blacken_object(vm, object);
    }
}

static void sweep(VM *vm) {
    Obj *previous = NULL;
    Obj *object = vm->objects;
    while (object != NULL) {
        if (object->marked) {
            object->marked = false;
#ifdef DEBUG
            printf("unmarking %p\n", object);
#endif
            previous = object;
            object = object->next;
        } else {
            Obj *unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm->objects = object;
            }

            free_obj(unreached);
        }
    }
}

void mark_roots(VM *vm) {
    for (Value *slot = vm->stack; slot < vm->stack + vm->stack_size; slot++) {
        mark_value(vm, *slot);
    }

    // current call frame
    CallFrame *frame = vm->call_frame;
    while (frame != NULL) {
        for (int i = 0; i < frame->locals_count; i++) {
            mark_value(vm, frame->locals[i]);
        }
        frame = frame->prev;
    }
}

void collect_garbage(VM *vm) {
    // printf("Collecting garbage...
    mark_roots(vm);
    trace_references(vm);
    sweep(vm);
    vm->nextGC = vm->allocated * GC_HEAP_GROW_FACTOR;
}

void *allocate(VM *vm, size_t size) {
    vm->allocated += size;
    void *ptr = malloc(size);

#ifdef DEBUG
    printf("allocated %p of size %ld\n", ptr, size);
#endif

    if (vm->allocated > vm->nextGC) {
        collect_garbage(vm);
    }

    return ptr;
}

void *reallocate(VM *vm, void *ptr, size_t oldSize, size_t newSize) {
    vm->allocated += newSize - oldSize;

#ifdef DEBUG
    printf("reallocating %p from %ld to %ld\n", ptr, oldSize, newSize);
#endif
    if (vm->allocated > vm->nextGC) {
        collect_garbage(vm);
    }

    void *new_ptr = realloc(ptr, newSize);

    return new_ptr;
}