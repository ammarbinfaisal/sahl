#ifndef OBJ_H

#define OBJ_H

#include "common.h"

#include <stdbool.h>
#include <stdint.h>

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define NANISH 0x7ffc000000000000
#define NANISH_MASK 0xffff000000000000
#define BOOLEAN_MASK 0x7ffe000000000002
#define INTEGER_MASK 0x7ffc000000000000
#define OBJECT_MASK 0xfffc000000000000
#define CHAR_MASK 0xfffe000000000000

#define TAG_FALSE 2 // 10.
#define TAG_TRUE 3  // 11.
#define TAG_INT 4   // 01.

#define IS_FLOAT(value) ((value & NANISH) != NANISH)
#define IS_OBJ(value) ((value & NANISH_MASK) == OBJECT_MASK)
#define IS_BOOL(v) ((v & BOOLEAN_MASK) == BOOLEAN_MASK)
#define IS_INT(v) ((v & NANISH_MASK) == INTEGER_MASK)
#define IS_CHAR(v) ((v & NANISH_MASK) == CHAR_MASK)

#define AS_BOOL(value) ((value) == TRUE_VAL)
#define AS_OBJ(value) ((Obj *)(value & 0xFFFFFFFFFFFF))
#define AS_FLOAT(value) value_to_float(value)
#define AS_CSTRING(value) (((AS_OBJ(value)))->string.data)
#define AS_INT(value) ((uint64_t)(value & 0xFFFFFFFFFF))
#define AS_CHAR(value) ((char)(value & 0xFF))

#define BOOL_VAL(b) ((b) ? TRUE_VAL : FALSE_VAL)
#define TRUE_VAL (BOOLEAN_MASK | 3)
#define FALSE_VAL (BOOLEAN_MASK | 2)
#define OBJ_VAL(obj) (Value)(OBJECT_MASK | (uint64_t)(obj))
#define INT_VAL(i) (Value)(INTEGER_MASK | (uint64_t)(i))
#define FLOAT_VAL(f) float_to_value(f)
#define CHAR_VAL(c) (Value)(CHAR_MASK | (uint64_t)(c))

double value_to_float(Value value);
Value float_to_value(double f);
void free_value(Value value);
void free_obj(Obj *obj);
Obj *new_obj(VM *vm, ObjType type);
bool obj_is_less(Value a, Value b);
bool obj_is_equal(Value a, Value b);
void print_value(Value value);

#endif
