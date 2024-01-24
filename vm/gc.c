#include "gc.h"
#include "common.h"
#include "list.h"
#include "rbtree.h"
#include "vm.h"
#include <complex.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define mark_value(vm, value) mark_obj(vm, (Obj *)value);

#define OOM_CHECK(cheney_state)                                                \
    do {                                                                       \
        if (cheney_state->top + sizeof(Obj) > cheney_state->extent) {          \
            printf("out of memory\n");                                         \
            exit(1);                                                           \
        }                                                                      \
    } while (0);

typedef struct {
    void *ptr;
    int size;
    bool obj;
} worklist_items_t;

void *checked_malloc(size_t size) {
    void *ptr = malloc(size);
    if (ptr == NULL) {
        printf("out of memory\n");
        exit(1);
    }
    return ptr;
}

void *cheney_allocate(VM *vm, int size) {
    CheneyState *cheney_state = vm->cheney_state;
    pthread_mutex_lock(&cheney_state->lock);
    if (cheney_state->free + size > cheney_state->top) {
        cheney_collect(vm->cheney_state, vm);
    }
    if (cheney_state->free + size > cheney_state->top) {
        // FIXME: heap expansion
        puts("out of memory");
        free_vm(vm);
        exit(1);
    }
    void *obj = cheney_state->free;
    cheney_state->free += size;
    pthread_mutex_unlock(&cheney_state->lock);
    return obj;
}

void cheney_collect(CheneyState *cs, VM *vm) {
    void *temp = cs->to_space;
    cs->to_space = cs->from_space;
    cs->from_space = temp;
    cs->top = cs->to_space + cs->extent;
    cs->free = cs->to_space;

    LinkedList *worklist = new_list();
    CallFrame *frame = vm->call_frame;

    while (frame != NULL) {
        StackMap *map = frame->stackmap;
        if (map) {
            for (int i = 0; i < map->len; i++) {
                for (int j = 0; j < 64; j++) {
                    if (map->bits[i] & (1ull << j)) {
                        worklist_items_t *item =
                            checked_malloc(sizeof(worklist_items_t));
                        item->ptr = frame->locals + i * 64 + j;
                        item->size = sizeof(Obj);
                        item->obj = true;
                        enqueue(worklist, item);
                    }
                }
            }
        }
        frame = frame->prev;
    }

    RBNode *forwarding_addr = new_rb_node(0);

    while (worklist->size) {
        worklist_items_t *w = dequeue(worklist);

        uint64_t **ptr = w->ptr;
        uint64_t *from_ref = *ptr;
        if (from_ref == NULL) {
            continue;
        }

        void *to_ref;
        RBNode *node = rb_search(forwarding_addr, (Value)from_ref);
        if (node != NULL) {
            *(void **)ptr = (void *)node->value;
        } else {
            // forward
            to_ref = cs->free;
            cs->free += w->size;
            memmove(to_ref, from_ref, w->size);
            *(void **)ptr = to_ref;
            RBNode *new_node = new_rb_node((Value)to_ref);
            new_node->key = (Value)from_ref;
            rb_insert(forwarding_addr, new_node);
        }

        if (w->obj) {
            Obj *obj = (Obj *)to_ref;

            switch (obj->type) {
            case OBJ_STRING: {
                break;
            }
            case OBJ_LIST: {
                if (obj->list.boxed_items) {
                    for (int i = 0; i < obj->list.length; i++) {
                        worklist_items_t *item =
                            checked_malloc(sizeof(worklist_items_t));
                        item->ptr = obj->list.items + i;
                        item->size = sizeof(Obj);
                        item->obj = true;
                        enqueue(worklist, item);
                    }
                }

                worklist_items_t *item =
                    checked_malloc(sizeof(worklist_items_t));
                item->ptr = &obj->list.items;
                item->size = sizeof(Value) * obj->list.capacity;
                item->obj = false;
                enqueue(worklist, item);

                break;
            }
            // FIXME: handle tuples with objs, maps with objs, variants, etc.
            }
        }

        free(w);
    }

    rb_free(forwarding_addr);

    free_linkedlist(worklist);
}
