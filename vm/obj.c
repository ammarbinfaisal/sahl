#include "obj.h"
#include "gc.h"
#include <stdlib.h>

double value_to_float(Value value) { return *(double *)&value; }

Value float_to_value(double f) { return *(Value *)&f; }

void free_value(Value value) {
    if (IS_OBJ(value)) {
        free_obj(AS_OBJ(value));
    }
}

Obj *new_obj(VM *vm, ObjType type) {
    Obj *obj = allocate(vm, sizeof(Obj));
    obj->type = type;
    obj->next = vm->objects;
    vm->objects = obj;
    obj->marked = false;
    return obj;
}

void free_obj(Obj *obj) {
#ifdef DEBUG
    printf("freeing %p\n\n", obj);
#endif

    if (obj->type == OBJ_LIST) {
#ifdef DEBUG
        printf("freeing list items %p\n", obj->list.items);
#endif
        free(obj->list.items);
    } else if (obj->type == OBJ_TUPLE) {
#ifdef DEBUG
        printf("freeing tuple items %p\n", obj->tuple.items);
#endif
        free(obj->tuple.items);
    } else if (obj->type == OBJ_STRING && !obj->string.constant) {
#ifdef DEBUG
        printf("freeing string chars %p\n", obj->string.data);
#endif
        free(obj->string.data);
    }

    free(obj);
}
