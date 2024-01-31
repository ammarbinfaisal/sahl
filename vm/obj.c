#include "obj.h"
#include "common.h"
#include "rbtree.h"
#include "treadmill/gc.h"
#include <pthread.h>
#include <stdlib.h>

double value_to_float(Value value) { return *(double *)&value; }

Value float_to_value(double f) { return *(Value *)&f; }

Obj *new_obj(VM *vm, ObjType type) {
    Obj *obj = (Obj *)Tm_allocate(vm->heap);
    obj->type = type;
    obj->marked = false;
    return obj;
}

void free_obj(Obj *obj) {
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
        if (!obj->string.constant) {
#ifdef DEBUGGC
            printf("freeing string chars %p\n", obj->string.data);
#endif
            free(obj->string.data);
        }
    } else if (obj->type == OBJ_MAP) {
        rb_free(obj->map.map);
    }
}