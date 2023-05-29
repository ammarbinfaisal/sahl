#include "obj.h"
#include "gc.h"
#include "rbtree.h"
#include <stdlib.h>
#include <string.h>

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
    } else if (obj->type == OBJ_MAP) {
        rb_free(obj->map.map);
    }

    free(obj);
}

bool obj_is_equal(Value a, Value b) {
#ifdef DEBUG
    printf("comparing ");
    print_value(a);
    printf(" and ");
    print_value(b);
    printf("\n");
#endif
    if (IS_OBJ(a) && IS_OBJ(b)) {
        Obj *obj_a = AS_OBJ(a);
        Obj *obj_b = AS_OBJ(b);

        return strcmp(obj_a->string.data, obj_b->string.data) == 0;
    }
    return a == b;
}

bool obj_is_less(Value a, Value b) {
    if (IS_FLOAT(a) && IS_FLOAT(b)) {
        return AS_FLOAT(a) < AS_FLOAT(b);
    }
    if (IS_INT(a) && IS_INT(b)) {
        return AS_INT(a) < AS_INT(b);
    }
    if (IS_OBJ(a) && IS_OBJ(b)) {
        Obj *obj_a = AS_OBJ(a);
        Obj *obj_b = AS_OBJ(b);

        // assume that the objects are string
        // that will be checked by the compiler
        return strcmp(obj_a->string.data, obj_b->string.data) < 0;
    } else {
        // obj is greater than non-obj
        if (IS_OBJ(a)) {
            return false;
        } else if (IS_OBJ(b)) {
            return true;
        } else {
            return a < b;
        }
    }
}

void print_value(Value value) {
    if (IS_BOOL(value)) {
        printf("%s", AS_BOOL(value) ? "true" : "false");
    } else if (IS_FLOAT(value)) {
        printf("%lf", AS_FLOAT(value));
    } else if (IS_INT(value)) {
        printf("%ld", AS_INT(value));
    } else if (IS_CHAR(value)) {
        printf("%c", AS_CHAR(value));
    } else if (IS_OBJ(value)) {
        Obj *obj = AS_OBJ(value);

#ifdef DEBUG
        printf("%p ", obj);
#endif

        if (obj->type == OBJ_STRING) {
            printf("%s", obj->string.data);
        } else if (obj->type == OBJ_LIST) {
            printf("[");
#ifndef MINARR
            for (int i = 0; i < obj->list.length; i++) {
                print_value(obj->list.items[i]);
                if (i != obj->list.length - 1) printf(", ");
            }
#else
            printf(" %ld items ", obj->list.length);
#endif
            printf("]");
        } else if (obj->type == OBJ_TUPLE) {
            printf("(");
            for (int i = 0; i < obj->tuple.length; i++) {
                print_value(obj->tuple.items[i]);
                if (i != obj->tuple.length - 1) printf(", ");
            }
            printf(")");
        } else { // MAP
            printf("{");
            RBNode *root = obj->map.map;
            LinkedList *list = rb_to_ll(root);
            LinkedList *curr = list;
            while (curr != NULL) {
                if (curr != list) {
                    print_value(curr->key);
                    printf(": ");
                    print_value(curr->value);
                    if (curr->next != NULL) printf(", ");
                }
                LinkedList *temp = curr;
                curr = curr->next;
                free(temp);
            }
            printf("}");
        }
    }
}