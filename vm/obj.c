#include "obj.h"
#include "common.h"
#include "gc.h"
#include "rbtree.h"
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

double value_to_float(Value value) { return *(double *)&value; }

Value float_to_value(double f) { return *(Value *)&f; }

Obj *new_obj(VM *vm, ObjType type) {
    Obj *obj = allocate(vm, sizeof(Obj));
    obj->type = type;
    obj->marked = false;
    return obj;
}

void track_obj(VM *vm, Obj *obj) {
    pthread_mutex_lock(&vm->gc_state->lock);
    obj->next = vm->gc_state->objects;
    vm->gc_state->objects = obj;
    pthread_mutex_unlock(&vm->gc_state->lock);
}

void free_obj(Obj *obj) {
    int free_it = 1;

    if (obj->type == OBJ_LIST) {
#ifdef DEBUGGC
        printf("freeing list items %p\n", obj->list.items);
#endif
        free(obj->list.items);
    } else if (obj->type == OBJ_TUPLE) {
#ifdef DEBUGGC
        printf("freeing tuple items %p\n", obj->tuple.items);
#endif
        free(obj->tuple.items);
    } else if (obj->type == OBJ_STRING) {
        if (obj->string.constant) {
            free_it = 0;
        } else {
#ifdef DEBUGGC
            printf("freeing string chars %p\n", obj->string.data);
#endif
            free(obj->string.data);
        }
    } else if (obj->type == OBJ_MAP) {
        rb_free(obj->map.map);
    }

    if (free_it) {
#ifdef DEBUGGC
        printf("freeing %p\n\n", obj);
#endif

        free(obj);
    }
}