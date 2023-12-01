#include "gc.h"
#include "common.h"
#include "conc.h"
#include "list.h"
#include "rbtree.h"
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define mark_value(vm, value) mark_obj(vm, (Obj *)value);

#define OOM_CHECK(cheney_state)                                                \
    do {                                                                       \
        if (cheney_state->from_top + sizeof(Obj) > cheney_state->extent) {     \
            printf("out of memory\n");                                         \
            exit(1);                                                           \
        }                                                                      \
    } while (0);

void *checked_malloc(size_t size) {
    void *ptr = malloc(size);
    if (ptr == NULL) {
        printf("out of memory\n");
        exit(1);
    }
    return ptr;
}

Obj *cheney_allocate(VM *vm, int count) {
    CheneyState *cheney_state = vm->cheney_state;
    pthread_mutex_lock(&cheney_state->lock);
    if (cheney_state->from_top + sizeof(Obj) >
        cheney_state->from_space + cheney_state->from_space_size) {
        // printf("from space exhausted, collecting garbage\n");
        pthread_mutex_unlock(&cheney_state->lock);
        cheney_collect(vm->cheney_state, vm);
        pthread_mutex_lock(&cheney_state->lock);
        OOM_CHECK(cheney_state)
    }
    Obj *obj = cheney_state->from_top;
    cheney_state->from_top += sizeof(Obj) * count;
    pthread_mutex_unlock(&cheney_state->lock);
    return obj;
}

void cheney_collect(CheneyState *cs, VM *vm) {
    pthread_mutex_lock(&cs->lock);
    memcpy(cs->from_space + cs->from_space_size, cs->from_space,
           cs->from_top - cs->from_space);
    cs->from_top = cs->from_space;

    pthread_mutex_unlock(&cs->lock);

    LinkedList *worklist = new_list();

    CallFrame *frame = vm->call_frame;
    while (frame != NULL) {
        StackMap *map = frame->stackmap;
        if (map) {
            for (int i = 0; i < map->len; i++) {
                for (int j = 0; j < 64; j++) {
                    if (map->bits[i] & (1 << j)) {
                        enqueue(worklist, (void *)frame->locals[i * 64 + j]);
                        Obj *obj = (Obj *)frame->locals[i * 64 + j];
                        obj->marked = true;
                    }
                }
            }
        }
        frame = frame->prev;
    }

    while (worklist->size) {
        Obj *obj = dequeue(worklist);
        // copy obj to to_space
        if (obj->marked) {
            continue;
        }
        obj->marked = true;

        memcpy(cs->from_top, obj, sizeof(Obj));
        cs->from_top += sizeof(Obj);

        switch (obj->type) {
        case OBJ_STRING: {
            break;
        }
        case OBJ_LIST: {
            if (obj->list.boxed_items) {
                for (int i = 0; i < obj->list.length; i++) {
                    enqueue(worklist, (void *)obj->list.items[i]);
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
                        enqueue(worklist, (void *)obj->tuple.items[i * 64 + j]);
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
                        enqueue(worklist, (void *)curr->key);
                    }
                    if (obj->map.value_boxed) {
                        enqueue(worklist, (void *)curr->value);
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
                    enqueue(worklist, (void *)n->value);
                }
            }
            break;
        }
        case OBJ_REF: {
            enqueue(worklist,
                    (void *)obj->ref.frame->locals[obj->ref.local_ix]);
            break;
        }
        }
    }

    free_linkedlist(worklist);
}
