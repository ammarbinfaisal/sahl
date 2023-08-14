#include "gc.h"
#include "common.h"
#include "conc.h"
#include "rbtree.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define mark_value(vm, value) mark_obj(vm, (Obj *)value);

static void mark_obj(VM *vm, Obj *obj) {
#ifdef DEBUGGC
    printf("marking %p\n", obj);
#endif
    if (!obj) return;
    if (obj->marked) return;
    obj->marked = true;

    if (vm->grayCapacity < vm->grayCount + 1) {
        vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
        vm->grayStack =
            (Obj **)realloc(vm->grayStack, sizeof(Obj *) * vm->grayCapacity);
    }

    vm->grayStack[vm->grayCount++] = obj;
}

static void blacken_object(VM *vm, Obj *obj) {
    switch (obj->type) {
    case OBJ_STRING: {
        break;
    }
    case OBJ_LIST: {
        if (obj->list.boxed_items) {
            for (int i = 0; i < obj->list.length; i++) {
                mark_value(vm, obj->list.items[i]);
            }
        }
        break;
    }
    case OBJ_TUPLE: {
        uint64_t *bitset = obj->tuple.boxed_items;
        int len = *bitset;
        bitset++;
        for (int i = 0; i < len; i++) {
            uint64_t bits = bitset[i];
            for (int j = 0; j < 64; j++) {
                if (bits & (1 << j)) {
                    mark_value(vm, obj->tuple.items[i * 64 + j]);
                }
            }
        }
        break;
    }
    case OBJ_MAP: {
        if (obj->map.key_boxed || obj->map.value_boxed) {
            LinkedListRB *ll = rb_to_ll(obj->map.map);
            LinkedListRB *curr = ll;
            while (curr != NULL) {
                if (obj->map.key_boxed) {
                    mark_value(vm, curr->key);
                }
                if (obj->map.value_boxed) {
                    mark_value(vm, curr->value);
                }
                curr = curr->next;
            }
        }

        break;
    }
    case OBJ_CHAN: {
        if (obj->channel.boxed_items) {
            Queue *q = obj->channel.chan->q;
            for (int i = 0; i < q->length; i++) {
                mark_value(vm, q->items[i]);
            }
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
    // current call frame
    CallFrame *frame = vm->call_frame;
    while (frame != NULL) {
        StackMap *map = frame->stackmap;
        if (map) {
            for (int i = 0; i < map->len; i++) {
                for (int j = 0; j < 64; j++) {
                    int pos = i * 64 + j;
                    if (pos >= frame->locals_count) goto nextframe;
                    if (map->bits[i] & (1 << j)) {
                        mark_value(vm, frame->locals[pos]);
                    }
                }
            }
        }
    nextframe:
        frame = frame->prev;
    }
}

void collect_garbage(VM *vm) {
    // printf("Collecting garbage... \n");
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