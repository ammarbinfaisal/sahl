#include <math.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "code.h"
#include "common.h"
#include "conc.h"
#include "debug.h"
#include "func.h"
#include "gc.h"
#include "obj.h"
#include "opcodes.h"
#include "rbtree.h"
#include "read.h"
#include "tcp.h"
#include "vm.h"

#define MAX_STACK 1024
#define MAX_CALL_DEPTH 1024
#define MAX_COROS 128

static int coro_count = 0;

void error(VM *vm, char *msg) {
    printf("Error: %s", msg);
    free_vm(vm);
    exit(1);
}

Value pop(VM *vm) {
#ifndef UNSAFE
    if (vm->stack_size == 0) {
        error(vm, "Stack underflow");
    }
#endif
    return vm->stack[--vm->stack_size];
}

Value peek(VM *vm) {
    if (vm->stack_size == 0) {
        error(vm, "Stack underflow");
    }
    return vm->stack[vm->stack_size - 1];
}

void push(VM *vm, Value value) {
#ifndef UNSAFE
    if (vm->stack_size == MAX_STACK) {
        error(vm, "Stack overflow");
    }
#endif
    vm->stack[vm->stack_size++] = value;
}

char *stringify(Value value) {
    char *str = calloc(1024, sizeof(char));
    int i = 0, l = 1024;
    if (IS_BOOL(value)) {
        sprintf(str, "%s", AS_BOOL(value) ? "true" : "false");
        return str;
    } else if (IS_FLOAT(value)) {
        sprintf(str, "%lf", AS_FLOAT(value));
        return str;
    } else if (IS_INT(value)) {
        sprintf(str, "%ld", AS_INT(value));
        return str;
    } else if (IS_OBJ(value)) {
        Obj *obj = AS_OBJ(value);
        if (obj->type == OBJ_STRING) {
            int len = strlen(obj->string.data);
            str = realloc(str, len + 1);
            sprintf(str, "%s", obj->string.data);
            return str;
        } else if (obj->type == OBJ_LIST) {
            i += sprintf(str, "[");
#ifndef MINARR
            for (int i = 0; i < obj->list.length; i++) {
                if (i > l / 2) {
                    l *= 2;
                    str = realloc(str, l);
                }
                i += sprintf(str, "%s%s", str, stringify(obj->list.items[i]));
                i += sprintf(str, "%s, ", str);
            }
#else
            sprintf(str, "%s %ld items ", str, obj->list.length);
#endif
            return str;
        } else if (obj->type == OBJ_TUPLE) {
            i += sprintf(str, "(");
            for (int i = 0; i < obj->tuple.length; i++) {
                if (i > l / 2) {
                    l *= 2;
                    str = realloc(str, l);
                }
                i += sprintf(str, "%s%s", str, stringify(obj->tuple.items[i]));
                if (i != obj->tuple.length - 1) sprintf(str, "%s, ", str);
            }
            sprintf(str, "%s)", str);
            return str;
        }
    }
    return str;
}

// Define function pointer type for opcodes
typedef void (*OpcodeHandler)(VM *);

void handle_iadd(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int64_t result = vm->regs[r1].i + vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_isub(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = vm->regs[r1].i - vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_imul(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = vm->regs[r1].i * vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_idiv(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = vm->regs[r1].i / vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_irem(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = vm->regs[r1].i % vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_ine(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    bool result = vm->regs[r1].i != vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_ieq(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    bool result = vm->regs[r1].i == vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_ilt(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    bool result = vm->regs[r1].i < vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
#ifdef DEBUG
    printf("handle_ilt: %ld < %ld = %d\n", vm->regs[r1].i, vm->regs[r2].i,
           result);
#endif
    vm->regs[res].i = result;
}

void handle_ile(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    bool result = vm->regs[r1].i <= vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_igt(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    bool result = vm->regs[r1].i > vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_ige(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    bool result = vm->regs[r1].i >= vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_fadd(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    double result = arg1 + arg2;
    vm->regs[res].i = *(uint64_t *)&result;
}

void handle_fsub(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    double result = arg1 - arg2;
    vm->regs[res].i = *(int *)&result;
}

void handle_fmul(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    double result = arg1 * arg2;
    vm->regs[res].i = *(int *)&result;
}

void handle_fdiv(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    double result = arg1 / arg2;
    vm->regs[res].i = *(int *)&result;
}

void handle_frem(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    double result = fmod(arg1, arg2);
    vm->regs[res].i = *(int *)&result;
}

void handle_fne(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    bool result = arg1 != arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_feq(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    bool result = arg1 == arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_flt(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    bool result = arg1 < arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_fle(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    bool result = arg1 <= arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_fgt(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    bool result = arg1 > arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_fge(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    double arg1 = *(double *)&vm->regs[r1].i;
    double arg2 = *(double *)&vm->regs[r2].i;
    bool result = arg1 >= arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_band(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = vm->regs[r1].i & vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_bor(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = vm->regs[r1].i | vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_bxor(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = vm->regs[r1].i ^ vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_bnot(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = ~vm->regs[r1].i;
    vm->regs[res].i = result;
}

void handle_land(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    bool result = vm->regs[r1].i && vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_lor(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    bool result = vm->regs[r1].i || vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_lnot(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    bool result = !vm->regs[r1].i;
    vm->regs[res].i = result;
}

void handle_bshl(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = vm->regs[r1].i << vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_bshr(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int result = vm->regs[r1].i >> vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_fneg(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    double result = -vm->regs[r1].f;
    vm->regs[r1].f = *(int *)&result;
}

void handle_ineg(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int result = -vm->regs[r1].i;
    vm->regs[r1].i = result;
}

void make_map(VM *vm, int reg, int len) {
    Obj *obj = new_obj(vm, OBJ_MAP);
    obj->map.map = new_rb_node(0);
    vm->regs[reg].i = (uint64_t)obj;
#ifdef DEBUG
    printf("make map %p - reg %d\n", obj, reg);
#endif
}

void make_list(VM *vm, int reg, int len) {
    Obj *obj = new_obj(vm, OBJ_LIST);
    size_t cap = GROW_CAPACITY(len);
    obj->list.items = malloc(sizeof(Value) * cap);
    obj->list.length = len;
    obj->list.capacity = cap;
    for (int i = 0; i < len; ++i) {
        obj->list.items[i] = 0;
    }
    vm->regs[reg].i = (uint64_t)obj;
#ifdef DEBUG
    printf("make list %p - reg %d\n", obj, reg);
#endif
}

void make_chan(VM *vm, int reg, int len) {
    Obj *obj = new_obj(vm, OBJ_CHAN);
    obj->channel.chan = new_chan(len);
    vm->regs[reg].i = (uint64_t)obj;
#ifdef DEBUG
    printf("make chan %p - reg %d\n", obj, reg);
#endif
}

typedef void (*MakeFn)(VM *vm, int reg, int len);

static MakeFn make_fns[] = {make_list, make_map, make_chan};

void handle_make(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int res = code[++vm->call_frame->ip];
    int len = code[++vm->call_frame->ip];
    int type = code[++vm->call_frame->ip] - 5;
    make_fns[type](vm, res, vm->regs[len].i);
}

void handle_listset(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int list = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int value = code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[list].i;
    int idx = vm->regs[index].i;
    obj->list.items[idx] = vm->regs[value].i;
}

void handle_listget(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int list = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[list].i;
    int idx = vm->regs[index].i;
    vm->regs[res].i = obj->list.items[idx];
}

void handle_mapset(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int map = code[++vm->call_frame->ip];
    int key = code[++vm->call_frame->ip];
    int value = code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[map].i;
    RBNode *new = new_rb_node(vm->regs[key].i);
    new->value = vm->regs[value].i;
    rb_insert(obj->map.map, new);
    rb_fixup(obj->map.map, new);
    obj->map.map->color = BLACK;
}

void handle_mapget(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int map = code[++vm->call_frame->ip];
    int key = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[map].i;
    RBNode *n = rb_search(obj->map.map, vm->regs[key].i);
#ifdef DEBUG
    printf("mapget %p %ld\n", obj, vm->regs[key].i);
#endif
    if (n) {
        vm->regs[res].i = n->value;
    } else {
        vm->regs[res].i = 0;
    }
}

void handle_map_to_list(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int map = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[map].i;
    Obj *newlist = map_to_list(vm, obj);
    vm->regs[res].i = (uint64_t)newlist;
}

void handle_list(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int len = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int res = code[vm->call_frame->ip];
    // pop len values from stack
    Obj *newobj = new_obj(vm, OBJ_LIST);
    newobj->list.length = len;
    newobj->list.capacity = len;
    newobj->list.items = malloc(sizeof(Value) * len);
    for (int i = 0; i < len; ++i) {
        newobj->list.items[i] = pop(vm);
    }
    vm->regs[res].i = (uint64_t)newobj;
}

void handle_tuple(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int len = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int res = code[vm->call_frame->ip];
    // pop len values from stack
    Obj *newobj = new_obj(vm, OBJ_TUPLE);
    newobj->tuple.length = len;
    newobj->tuple.items = malloc(sizeof(Value) * len);
    for (int i = 0; i < len; ++i) {
        newobj->tuple.items[i] = pop(vm);
    }
    vm->regs[res].i = (uint64_t)newobj;
}

void handle_tupleget(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int tuple = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[tuple].i;
    vm->regs[res].i = obj->tuple.items[vm->regs[index].i];
}

void handle_chansend(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int chan = code[++vm->call_frame->ip];
    int value = code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[chan].i;
    chan_write(obj->channel.chan, vm->regs[value].i);
}

void handle_chanrecv(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int chan = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[chan].i;
    chan_read(obj->channel.chan, (Value *)&vm->regs[res].i);
}

void handle_strget(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int str = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[str].i;
    vm->regs[res].i = obj->string.data[index];
}

void handle_jmp(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int offset = read_u64(code, vm->call_frame->ip + 1);
    vm->call_frame->ip = offset - 1;
}

void handle_jmpifnot(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int cond = code[++vm->call_frame->ip];
    int offset = read_u64(code, ++vm->call_frame->ip);
    if (vm->regs[cond].i) {
#ifdef DEBUG
        printf("jmpifnot: condition is true, not jumping\n");
#endif
        vm->call_frame->ip += 7;
    } else {
        vm->call_frame->ip = offset - 1;
    }
}

void handle_call(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int func = read_u64(code, vm->call_frame->ip + 1);
    int nargs = read_u64(code, vm->call_frame->ip + 9);
    // read arg registers and put them in local registers of the new call frame
    CallFrame *cf = new_call_frame(vm->funcs + func, vm->call_frame);
    cf->locals_count = nargs;
    cf->locals_capacity = GROW_CAPACITY(nargs);
    cf->locals = malloc(sizeof(Value) * cf->locals_capacity);
    for (int i = 0; i < nargs; ++i) {
        int reg = code[vm->call_frame->ip + 17 + i];
        cf->locals[i] = vm->regs[reg].i;
    }
    vm->call_frame->ip +=
        16 + nargs; // 16 instead of 17 because we pre-increment ip again
    vm->call_frame = cf;
    vm->call_frame->ip = -1;
}

typedef void (*native_fn_t)(VM *vm);

void native_print_int(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7; // 7 instead of 8 because we pre-increment ip again
    // read regs and print them
    for (int i = 0; i < argc; ++i) {
        int arg = code[++vm->call_frame->ip];
        printf("%ld", vm->regs[arg].i);
    }
}

void native_print_float(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7; // 7 instead of 8 because we pre-increment ip again
    // read regs and print them
    for (int i = 0; i < argc; ++i) {
        int arg = code[++vm->call_frame->ip];
        printf("%lf", *(double *)&vm->regs[arg].i);
    }
}

void native_print_char(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7; // 7 instead of 8 because we pre-increment ip again
    // read regs and print them
    for (int i = 0; i < argc; ++i) {
        int arg = code[++vm->call_frame->ip];
        printf("%c", (char)vm->regs[arg].i);
    }
}

void native_print_bool(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7; // 7 instead of 8 because we pre-increment ip again
    // read regs and print them
    for (int i = 0; i < argc; ++i) {
        int arg = code[++vm->call_frame->ip];
        printf("%s", vm->regs[arg].i ? "true" : "false");
    }
}

void native_print_str(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7; // 7 instead of 8 because we pre-increment ip again
    // read regs and print them
    for (int i = 0; i < argc; ++i) {
        int arg = code[++vm->call_frame->ip];
        Obj *obj = (Obj *)vm->regs[arg].i;
        printf("%s", obj->string.data);
    }
}

void native_append(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7; // 7 instead of 8 because we pre-increment ip again
    int list = vm->call_frame->func->code[++vm->call_frame->ip];
    int value = vm->call_frame->func->code[++vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[list].i;
    if (obj->list.length == obj->list.capacity) {
        obj->list.capacity = GROW_CAPACITY(obj->list.capacity);
        obj->list.items =
            realloc(obj->list.items, sizeof(Value) * obj->list.capacity);
    }
    obj->list.items[obj->list.length] = vm->regs[value].i;
    ++obj->list.length;
#ifdef DEBUG
    printf("appended %ld to list of length %ld\n", vm->regs[value].i,
           obj->list.length);
#endif
}

void native_list_len(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8; // 7 instead of 8 because we pre-increment ip again
    int list = code[vm->call_frame->ip];
    Obj *obj = (Obj *)vm->regs[list].i;
    vm->regs[0].i = obj->list.length;
}

static native_fn_t native_functions[] = {
    native_print_int, native_print_float, native_print_char, native_print_bool,
    native_print_str, native_append,      native_list_len};

void handle_ncall(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int func = code[++vm->call_frame->ip];
    native_fn_t fn = native_functions[func];
    fn(vm);
}

void handle_const(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int index = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int reg = code[vm->call_frame->ip];
    vm->regs[reg].i = vm->consts[index];
#ifdef DEBUG
    printf("const %ld loaded into reg %d\n", (int64_t)vm->consts[index], reg);
#endif
}

void handle_load(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int index = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int reg = code[vm->call_frame->ip];
    vm->regs[reg].i = vm->call_frame->locals[index];
#ifdef DEBUG
    printf("local %d (%ld) loaded into reg %d\n", index,
           vm->call_frame->locals[index], reg);
#endif
}

void handle_store(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int index = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int reg = code[vm->call_frame->ip];
    if (index >= vm->call_frame->locals_capacity) {
        vm->call_frame->locals_capacity = index + 1;
        vm->call_frame->locals =
            realloc(vm->call_frame->locals,
                    sizeof(Value) * vm->call_frame->locals_capacity);
    }
    if (index >= vm->call_frame->locals_count) {
        vm->call_frame->locals_count = index + 1;
    }
    vm->call_frame->locals[index] = vm->regs[reg].i;
#ifdef DEBUG
    printf("reg %d (%ld) stored into local %d\n", reg, vm->regs[reg].i, index);
#endif
}

const int TYPE_INT = 0;
const int TYPE_FLOAT = 1;
const int TYPE_BOOL = 2;
const int TYPE_CHAR = 3;

void handle_cast(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int ty1 = code[++vm->call_frame->ip];
    int ty2 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    if (ty1 == ty2) {
        vm->regs[r2].i = vm->regs[r1].i;
    } else if (ty1 == TYPE_INT && ty2 == TYPE_FLOAT) {
        vm->regs[r2].i = (double)vm->regs[r1].i;
    } else if (ty1 == TYPE_FLOAT && ty2 == TYPE_INT) {
        vm->regs[r2].i = (int64_t)vm->regs[r1].i;
    } else if (ty1 == TYPE_INT && ty2 == TYPE_BOOL) {
        vm->regs[r2].i = vm->regs[r1].i != 0;
    } else if (ty1 == TYPE_BOOL && ty2 == TYPE_INT) {
        vm->regs[r2].i = vm->regs[r1].i;
    } else if (ty1 == TYPE_CHAR && ty2 == TYPE_INT) {
        vm->regs[r2].i = vm->regs[r1].i;
    } else if (ty1 == TYPE_INT && ty2 == TYPE_CHAR) {
        vm->regs[r2].i = (char)vm->regs[r1].i;
    }
}

void handle_move(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int r2 = code[++vm->call_frame->ip];
    vm->regs[r1].i = vm->regs[r2].i;
#ifdef DEBUG
    printf("reg %d (%ld) moved into reg %d\n", r2, vm->regs[r2].i, r1);
#endif
}

void handle_ret(VM *vm) {
    if (vm->call_frame->prev) {
        CallFrame *curr = vm->call_frame;
        vm->call_frame = vm->call_frame->prev;
        free_call_frame(curr);
    } else {
        free_vm(vm);
        exit(0);
    }
}

void handle_return(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r = code[++vm->call_frame->ip];
    vm->regs[0].i = vm->regs[r].i;
    handle_ret(vm);
}

void handle_push(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r = code[++vm->call_frame->ip];
    push(vm, vm->regs[r].i);
}

void handle_pop(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r = code[++vm->call_frame->ip];
    vm->regs[r].i = pop(vm);
}

void handle_spawn(VM *vm) { ++vm->call_frame->ip; }

void handle_nop(VM *vm) { ++vm->call_frame->ip; }

// Create function pointer table for opcodes
static OpcodeHandler opcode_handlers[] = {
    handle_iadd,   handle_isub,     handle_imul,     handle_idiv,
    handle_irem,   handle_ine,      handle_ieq,      handle_ilt,
    handle_ile,    handle_igt,      handle_ige,      handle_fadd,
    handle_fsub,   handle_fmul,     handle_fdiv,     handle_frem,
    handle_fne,    handle_feq,      handle_flt,      handle_fle,
    handle_fgt,    handle_fge,      handle_band,     handle_bor,
    handle_bxor,   handle_bnot,     handle_land,     handle_lor,
    handle_lnot,   handle_bshl,     handle_bshr,     handle_fneg,
    handle_ineg,   handle_make,     handle_listset,  handle_listget,
    handle_list,   handle_tupleget, handle_tuple,    handle_strget,
    handle_mapget, handle_mapset,   handle_chansend, handle_chanrecv,
    handle_jmp,    handle_jmpifnot, handle_call,     handle_ncall,
    handle_const,  handle_load,     handle_store,    handle_cast,
    handle_move,   handle_return,   handle_push,     handle_pop,
    handle_spawn,  handle_nop,      handle_ret};

void run(VM *vm) {
    while (vm->call_frame->ip < vm->call_frame->func->code_length) {
        uint8_t instruction = vm->call_frame->func->code[vm->call_frame->ip];

#ifdef PRINT_OPCODES
        printf("%d ", vm->call_frame->ip);
        print_opcode(vm->call_frame->func->code, vm->call_frame->ip);
#endif

#ifdef PRINT_STACK
        printf("Stack: ");
        for (int i = 0; i < vm->stack_size; ++i) {
            print_value(vm->stack[i]);
            printf(" ");
        }
        printf(" (size: %d)\n", vm->stack_size);
#endif

#ifdef PRINT_LOCALS
        printf("Locals: ");
        for (int j = 0; j < vm->call_frame->locals_count; ++j) {
            print_value(vm->call_frame->locals[j]);
            printf(" ");
        }
        printf(" (size: %d)\n", vm->call_frame->locals_count);
#endif

#ifndef UNSAFE
        if (instruction >= NUM_OPCODES) {
            error(vm, "Invalid opcode");
        }
#endif

        opcode_handlers[instruction](vm);

        vm->call_frame->ip++;
    }
}

int main(int argc, char **argv) {
    if (argc != 2) {
        return 1;
    }
    const char *filename = argv[1];
    Code *code = read_bytecode(filename);
    printf("length %ld\n", code->length);
    // dissassemble(code->bytes, code->length);
    puts("\n\n\n");
    VM *vm = new_vm(code->bytes, code->length);
    run(vm);
    // free(code->bytes);
    free(code);
    free_vm(vm);
}
