#ifndef OBJ_H

#define OBJ_H

#include "common.h"

#include <stdbool.h>
#include <stdint.h>

double value_to_float(Value value);
Value float_to_value(double f);
void free_obj(Obj *obj);
Obj *new_obj(VM *vm, ObjType type);

#endif
