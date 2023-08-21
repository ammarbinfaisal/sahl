#include "gc.h"
#include "common.h"
#include "conc.h"
#include "list.h"
#include "rbtree.h"
#include <pthread.h>
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

    GCState *gc_state = vm->gc_state;

    if (gc_state->grayCapacity < gc_state->grayCount + 1) {
        gc_state->grayCapacity = GROW_CAPACITY(gc_state->grayCapacity);
        gc_state->grayStack = (Obj **)realloc(
            gc_state->grayStack, sizeof(Obj *) * gc_state->grayCapacity);
    }

    gc_state->grayStack[gc_state->grayCount++] = obj;
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
            LinkedList *q = obj->channel.chan->q;
            Node *n = q->head;
            while (n->next != NULL) {
                mark_value(vm, n->value);
            }
        }
        break;
    }
    }
}

static void trace_references(VM *vm) {
    while (vm->gc_state->grayCount > 0) {
        Obj *object = vm->gc_state->grayStack[--vm->gc_state->grayCount];
        blacken_object(vm, object);
    }
}

static void sweep(VM *vm) {
    Obj *previous = NULL;
    GCState *gc_state = vm->gc_state;
    Obj *object = gc_state->objects;
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
                gc_state->objects = object;
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
#ifdef USE_GC
    if (vm->is_coro) {
        return;
    }
    mark_roots(vm);
    trace_references(vm);
    sweep(vm);
    vm->gc_state->nextGC = vm->gc_state->allocated * GC_HEAP_GROW_FACTOR;
#endif
}

void *allocate(VM *vm, size_t size) {
    void *ptr = malloc(size);

#ifdef DEBUG
    printf("allocated %p of size %ld\n", ptr, size);
#endif

    pthread_mutex_lock(&vm->gc_state->lock);
    vm->gc_state->allocated += size;

    if (vm->gc_state->allocated > vm->gc_state->nextGC) {
        collect_garbage(vm);
    }
    pthread_mutex_unlock(&vm->gc_state->lock);

    return ptr;
}

void *reallocate(VM *vm, void *ptr, size_t oldSize, size_t newSize) {
    pthread_mutex_lock(&vm->gc_state->lock);
    vm->gc_state->allocated += newSize - oldSize;

    if (vm->gc_state->allocated > vm->gc_state->nextGC) {
        collect_garbage(vm);
    }
    pthread_mutex_unlock(&vm->gc_state->lock);

#ifdef DEBUG
    printf("reallocating %p from %ld to %ld\n", ptr, oldSize, newSize);
#endif

    void *new_ptr = realloc(ptr, newSize);

    return new_ptr;
}