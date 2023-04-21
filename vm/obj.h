#ifndef OBJ_H

#define OBJ_H

#include "common.h"

#include <stdbool.h>
#include <stdint.h>

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN ((uint64_t)0x7ffc000000000000)

#define TAG_FALSE 2 // 10.
#define TAG_TRUE 3  // 11.

#define IS_BOOL(value) (((value) | 1) == TRUE_VAL)
#define IS_NUMBER(value) (((value)&QNAN) != QNAN)
#define IS_OBJ(value) (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value) ((value) == TRUE_VAL)
#define AS_OBJ(value) ((Obj *)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))
#define AS_FLOAT(value) value_to_float(value)

#define BOOL_VAL(b) ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define OBJ_VAL(obj) (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))
#define FLOAT_VAL(f) float_to_value(f)

double value_to_float(Value value);
Value float_to_value(double f);
void free_value(Value value);
void free_obj(Obj *obj);
Obj *new_obj(VM *vm, ObjType type);
bool obj_is_less(Value a, Value b);
bool obj_is_equal(Value a, Value b);
void print_value(Value value);

#endif
