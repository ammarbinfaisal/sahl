#include <math.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "code.h"
#include "common.h"
#include "conc.h"
#include "debug.h"
#include "func.h"
#include "gc.h"
#include "list.h"
#include "obj.h"
#include "opcodes.h"
#include "rbtree.h"
#include "read.h"
#include "scheduler.h"
#include "treadmill/gc.h"
#include "vm.h"

#define MAX_STACK 1024
#define MAX_CALL_DEPTH 1024
#define MAIN_ID 0xc001beef

Scheduler *scheduler;

static void push_scheduler(VM *corovm) {
    enqueue(scheduler->coro_queue, corovm);
}

static VM *pop_scheduler() {
    pthread_mutex_lock(&scheduler->vmq_mu);
    while (!scheduler->coro_queue->size) {
        pthread_cond_wait(&scheduler->vmq_cond, &scheduler->vmq_mu);
    }
    VM *vm = dequeue(scheduler->coro_queue);
    pthread_mutex_unlock(&scheduler->vmq_mu);
    return vm;
}

void error(VM *vm, char *msg) {
    printf("Error: %s", msg);
    // free_vm(vm);
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

// Define function pointer type for opcodes
typedef void (*OpcodeHandler)(VM *);

#define OP_R1_R2_RES                                                           \
    uint8_t *code = vm->call_frame->func->code;                                \
    int r1 = code[++vm->call_frame->ip];                                       \
    int r2 = code[++vm->call_frame->ip];                                       \
    int res = code[++vm->call_frame->ip];

#define OP_CODE_R1_R2                                                          \
    uint8_t *code = vm->call_frame->func->code;                                \
    int r1 = code[++vm->call_frame->ip];                                       \
    int r2 = code[++vm->call_frame->ip];

#define DOUBLE_ARG1_ARG2                                                       \
    double arg1 = vm->regs[r1].f;                                              \
    double arg2 = vm->regs[r2].f;

void handle_iadd(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i + vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_isub(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i - vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_imul(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i * vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_idiv(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i / vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_irem(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i % vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_ine(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i != vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_ieq(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i == vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_ilt(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i < vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
#ifdef DEBUG
    printf("handle_ilt: %ld < %ld = %d\n", vm->regs[r1].i, vm->regs[r2].i,
           result);
#endif
    vm->regs[res].i = result;
}

void handle_ile(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i <= vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_igt(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i > vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_ige(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i >= vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_fadd(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = arg1 + arg2;
    vm->regs[res].i = *(uint64_t *)&result;
}

void handle_fsub(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = arg1 - arg2;
    vm->regs[res].i = *(int64_t *)&result;
}

void handle_fmul(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = arg1 * arg2;
    vm->regs[res].i = *(int64_t *)&result;
}

void handle_fdiv(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = arg1 / arg2;
    vm->regs[res].i = *(int64_t *)&result;
}

void handle_frem(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = fmod(arg1, arg2);
    vm->regs[res].i = *(int64_t *)&result;
}

void handle_fne(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 != arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_feq(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 == arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_flt(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 < arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_fle(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 <= arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_fgt(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 > arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_fge(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 >= arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

void handle_band(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i & vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_bor(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i | vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_bxor(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i ^ vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_bnot(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int64_t result = ~vm->regs[r1].i;
    vm->regs[res].i = result;
}

void handle_land(VM *vm) {
    OP_R1_R2_RES
    bool result = vm->regs[r1].i && vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_lor(VM *vm) {
    OP_R1_R2_RES
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
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i << vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_bshr(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i >> vm->regs[r2].i;
    vm->regs[res].i = result;
}

void handle_fneg(VM *vm) {
    OP_CODE_R1_R2
    double result = -vm->regs[r1].f;
    vm->regs[r2].f = *(int64_t *)&result;
}

void handle_ineg(VM *vm) {
    OP_CODE_R1_R2
    int64_t result = -vm->regs[r1].i;
    vm->regs[r2].i = result;
}

void make_map(VM *vm, int reg, int _) {
    uint8_t *code = vm->call_frame->func->code;
    Obj *obj = new_obj(vm, OBJ_MAP);
    obj->map.map = new_rb_node(0);
    vm->regs[reg].i = (uint64_t)obj;
    uint8_t key_boxed = code[++vm->call_frame->ip];
    uint8_t value_heap_alloc = code[++vm->call_frame->ip];
    obj->map.key_boxed = key_boxed;
    obj->map.value_boxed = value_heap_alloc;
#ifdef DEBUG
    printf("make map %p - reg %d\n", obj, reg);
#endif
}

void make_list(VM *vm, int reg, int len) {
    Obj *obj = new_obj(vm, OBJ_LIST);
    size_t cap = GROW_CAPACITY(len);
    obj->list.items = checked_malloc(sizeof(Value) * cap);
    obj->list.length = len;
    obj->list.capacity = cap;
    vm->regs[reg].i = (uint64_t)obj;
    obj->list.boxed_items = vm->call_frame->func->code[++vm->call_frame->ip];
#ifdef DEBUG
    printf("make list %p - reg %d\n", obj, reg);
#endif
}

void make_chan(VM *vm, int reg, int len) {
    Obj *obj = new_obj(vm, OBJ_CHAN);
    obj->channel.chan = new_chan(len ? len : 2);
    vm->regs[reg].i = (uint64_t)obj;
    obj->list.boxed_items = vm->call_frame->func->code[++vm->call_frame->ip];
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
    Obj *obj = vm->regs[list].o;
    int idx = vm->regs[index].i;
    if (idx >= obj->list.length) {
        char msg[100];
        sprintf(msg, "Index %d out of bounds for list of length %ld", idx,
                obj->list.length);
        error(vm, msg);
    }
    obj->list.items[idx] = vm->regs[value].i;
}

void handle_listget(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int list = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[list].o;
    int idx = vm->regs[index].i;
    if (idx >= obj->list.length) {
        char msg[100];
        sprintf(msg, "Index %d out of bounds for list of length %ld", idx,
                obj->list.length);
        error(vm, msg);
    }
    vm->regs[res].i = obj->list.items[idx];
}

void handle_mapset(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int map = code[++vm->call_frame->ip];
    int key = code[++vm->call_frame->ip];
    int value = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[map].o;
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
    Obj *obj = vm->regs[map].o;
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

void handle_list(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int len = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint8_t heap_alloced = code[vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    // pop len values from stack
    Obj *newobj = new_obj(vm, OBJ_LIST);
    newobj->list.length = len;
    newobj->list.capacity = len;
    newobj->list.items = checked_malloc(sizeof(Value) * len);
    newobj->list.boxed_items = heap_alloced;
    for (int i = 0; i < len; ++i) {
        newobj->list.items[i] = pop(vm);
    }
    vm->regs[res].i = (uint64_t)newobj;
}

void handle_tuple(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int len = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint64_t bitsets_count = read_u64(code, vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint64_t *bitsets = checked_malloc(sizeof(uint64_t) * bitsets_count + 1);
    *bitsets = bitsets_count; // store count in first element
    for (int i = 0; i < bitsets_count; ++i) {
        bitsets[i + 1] = read_u64(code, vm->call_frame->ip);
        vm->call_frame->ip += 8;
    }
    int res = code[vm->call_frame->ip];
    // pop len values from stack
    Obj *newobj = new_obj(vm, OBJ_TUPLE);
    newobj->tuple.length = len;
    newobj->tuple.items = checked_malloc(sizeof(Value) * len);
    // now, tupleset will be used to set the values
    // for (int i = 0; i < len; ++i) {
    //     newobj->tuple.items[i] = pop(vm);
    // }
    newobj->tuple.boxed_items = bitsets;
    vm->regs[res].i = (uint64_t)newobj;
}

void handle_tupleset(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int tuple = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int reg = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[tuple].o;
    obj->tuple.items[vm->regs[index].i] = vm->regs[reg].i;
}

void handle_tupleget(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int tuple = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[tuple].o;
    vm->regs[res].i = obj->tuple.items[vm->regs[index].i];
}

void handle_chansend(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    uint64_t chan_var = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int value = code[vm->call_frame->ip];
    Obj *obj = (Obj *)vm->call_frame->locals[chan_var];
#ifdef DEBUG
    printf("sending %ld over chan %p\n", vm->regs[value].i, obj);
#endif
    Chan *chan = obj->channel.chan;
    pthread_mutex_lock(&chan->m_mu);
    chan_write(chan, vm->regs[value].i);
    // printf("waiting send sent: %p %x\n", chan, vm->coro_id);
    int len = chan->q->size;
    if (chan->r_waiting > 0) {
        pthread_cond_signal(&chan->r_cond);
    }
    if (len == chan->cap) {
        vm->should_yield = true;
    }
    pthread_mutex_unlock(&chan->m_mu);
}

void handle_chanrecv(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int ip = vm->call_frame->ip;
    uint64_t chan_var = read_u64(code, ++ip);
    ip += 8;
    int res = code[ip];
    Obj *obj = (Obj *)vm->call_frame->locals[chan_var];
    Chan *chan = obj->channel.chan;
    // printf("waiting recv: %p %x\n", chan, vm->coro_id);
    pthread_mutex_lock(&chan->m_mu);
    while (chan->q->size == 0) {
        chan->r_waiting++;
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        // 1000 nanoseconds = 1 microsecond
        ts.tv_nsec += 100;
        pthread_cond_timedwait(&chan->r_cond, &chan->m_mu, &ts);
        chan->r_waiting--;
        // if we are not the main thread, we should yield
        if (vm->coro_id != MAIN_ID) {
            break;
        }
    }
    if (chan->len == 0) {
        // printf("giving up waiting recv : %p %x\n", chan, vm->coro_id);
        pthread_mutex_unlock(&chan->m_mu);
        vm->should_yield = true;
        vm->call_frame->ip--; // as we will yield after the ip increment
                              // and we want to re-execute this instruction
        return;
    }
    chan_read(chan, (Value *)&vm->regs[res].i);
    // printf("waiting recv read: %p %x\n", chan, vm->coro_id);
    pthread_mutex_unlock(&chan->m_mu);
    vm->call_frame->ip = ip;
}

void handle_strget(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int str = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[str].o;
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

    CallFrame *cf;
    cf = new_call_frame(vm->funcs + func, vm->call_frame);
    cf->locals_count = nargs;
    cf->locals_capacity = GROW_CAPACITY(nargs);
    cf->locals = checked_malloc(sizeof(Value) * cf->locals_capacity);
    for (int i = 0; i < nargs; ++i) {
        int reg = code[vm->call_frame->ip + 17 + i];
        cf->locals[i] = vm->regs[reg].i;
    }
    vm->call_frame->ip +=
        16 + nargs; // 16 instead of 17 because we pre-increment ip again
    vm->call_frame = cf;
    vm->call_frame->ip = -1;
}

void handle_corocall(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int func = read_u64(code, vm->call_frame->ip + 1);
    int nargs = read_u64(code, vm->call_frame->ip + 9);
    // read arg registers and put them in local registers of the new call frame
    VM *vmm = coro_vm(vm, func);
    CallFrame *cf = vmm->call_frame;
    cf->locals_count = nargs;
    cf->locals_capacity = GROW_CAPACITY(nargs);
    cf->locals = checked_malloc(sizeof(Value) * cf->locals_capacity);
    for (int i = 0; i < nargs; ++i) {
        int reg = code[vm->call_frame->ip + 17 + i];
        cf->locals[i] = vm->regs[reg].i;
    }
    vm->call_frame->ip +=
        16 + nargs; // 16 instead of 17 because we pre-increment ip again
    cf->ip = 0;
    pthread_mutex_lock(&scheduler->vmq_mu);
    push_scheduler(vmm);
    pthread_cond_broadcast(&scheduler->vmq_cond);
    pthread_mutex_unlock(&scheduler->vmq_mu);
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
        printf("%lf", vm->regs[arg].f);
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
        Obj *obj = vm->regs[arg].o;
        printf("%s", obj->string.data);
    }
}

void native_append(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7; // 7 instead of 8 because we pre-increment ip again
    int list = vm->call_frame->func->code[++vm->call_frame->ip];
    int value = vm->call_frame->func->code[++vm->call_frame->ip];
    Obj *obj = vm->regs[list].o;
    if (obj->list.length == obj->list.capacity) {
        uint64_t old_cap = obj->list.capacity;
        obj->list.capacity = GROW_CAPACITY(obj->list.capacity);
        void *old = obj->list.items;
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
    Obj *obj = vm->regs[list].o;
    vm->regs[0].i = obj->list.length;
}

void native_pop(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int list = code[vm->call_frame->ip];
    Obj *obj = vm->regs[list].o;
    vm->regs[0].i = obj->list.items[obj->list.length];
    obj->list.length--;
}

void native_make_variant(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int reg = code[vm->call_frame->ip];
    int variant = code[++vm->call_frame->ip];
    Obj *obj = new_obj(vm, OBJ_VARIANT);
    obj->variant.tag = vm->regs[variant].i;
    obj->variant.value = vm->regs[reg].i;
    vm->regs[0].i = (uint64_t)obj;
}

void native_is_variant(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int variant = code[vm->call_frame->ip];
    int reg = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[variant].o;
    vm->regs[0].i = obj->variant.tag == vm->regs[reg].i;
}

void native_get_variant(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int argc = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int variant = code[vm->call_frame->ip];
    Obj *obj = vm->regs[variant].o;
    vm->regs[0].i = obj->variant.value;
}

static native_fn_t native_functions[] = {
    native_print_int,  native_print_float, native_print_char,
    native_print_bool, native_print_str,   native_append,
    native_list_len,   native_pop,         native_make_variant,
    native_is_variant, native_get_variant};

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
    if (index >= vm->call_frame->locals_count) {
        vm->call_frame->locals_count = index + 1;
        if (vm->call_frame->locals_count == vm->call_frame->locals_capacity) {
            vm->call_frame->locals_capacity = GROW_CAPACITY(index + 1);
            vm->call_frame->locals =
                realloc(vm->call_frame->locals,
                        sizeof(Value) * vm->call_frame->locals_capacity);
        }
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
        double res = (double)vm->regs[r1].i;
        vm->regs[r2].i = *(int64_t *)&res;
    } else if (ty1 == TYPE_FLOAT && ty2 == TYPE_INT) {
        double res = *(double *)&vm->regs[r1].i;
        vm->regs[r2].i = (int64_t)res;
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
    OP_CODE_R1_R2
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

void handle_spawn(VM *vm) {
    // this instruction is deprecated
}

void handle_nop(VM *vm) { ; }

void handle_stack_map(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    uint64_t len = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    StackMap *stmap = checked_malloc(sizeof(StackMap));
    stmap->len = len;
    stmap->bits = checked_malloc(len * sizeof(uint64_t));
    uint64_t *bitptr = stmap->bits;
    while (len--) {
        *bitptr = read_u64(code, vm->call_frame->ip);
        vm->call_frame->ip += 8;
        bitptr++;
    }
    if (vm->call_frame->stackmap) {
        free(vm->call_frame->stackmap->bits);
        free(vm->call_frame->stackmap);
    }
    vm->call_frame->stackmap = stmap;
    --vm->call_frame->ip;
}

// super instructions
Value op_iadd(Value v1, Value v2) { return v1 + v2; }

Value op_isub(Value v1, Value v2) { return v1 - v2; }

Value op_imul(Value v1, Value v2) { return v1 * v2; }

Value op_idiv(Value v1, Value v2) { return v1 / v2; }

Value op_irem(Value v1, Value v2) { return v1 % v2; }

Value op_ine(Value v1, Value v2) { return v1 != v2; }

Value op_ieq(Value v1, Value v2) { return v1 == v2; }

Value op_ilt(Value v1, Value v2) { return v1 < v2; }

Value op_ile(Value v1, Value v2) { return v1 <= v2; }

Value op_igt(Value v1, Value v2) { return v1 > v2; }

Value op_ige(Value v1, Value v2) { return v1 >= v2; }

Value op_fadd(Value v1, Value v2) {
    double res = (AS_DOUBLE(v1) + AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

Value op_fsub(Value v1, Value v2) {
    double res = (AS_DOUBLE(v1) - AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

Value op_fmul(Value v1, Value v2) {
    double res = (AS_DOUBLE(v1) * AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

Value op_fdiv(Value v1, Value v2) {
    double res = (AS_DOUBLE(v1) / AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

Value op_frem(Value v1, Value v2) {
    double res = fmod(AS_DOUBLE(v1), AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

Value op_feq(Value v1, Value v2) { return AS_DOUBLE(v1) == AS_DOUBLE(v2); }

Value op_flt(Value v1, Value v2) { return AS_DOUBLE(v1) < AS_DOUBLE(v2); }

Value op_fle(Value v1, Value v2) { return AS_DOUBLE(v1) <= AS_DOUBLE(v2); }

Value op_fgt(Value v1, Value v2) { return AS_DOUBLE(v1) > AS_DOUBLE(v2); }

Value op_fge(Value v1, Value v2) { return AS_DOUBLE(v1) >= AS_DOUBLE(v2); }

Value op_fne(Value v1, Value v2) { return AS_DOUBLE(v1) != AS_DOUBLE(v2); }

Value op_band(Value v1, Value v2) { return v1 & v2; }

Value op_bor(Value v1, Value v2) { return v1 | v2; }

Value op_bxor(Value v1, Value v2) { return v1 ^ v2; }

Value op_land(Value v1, Value v2) { return v1 && v2; }

Value op_lor(Value v1, Value v2) { return v1 || v2; }

Value op_bshl(Value v1, Value v2) { return v1 << v2; }

Value op_bshr(Value v1, Value v2) { return v1 >> v2; }

Value op_dummy(Value _1, Value _2) {
    printf("sigill\n");
    exit(1);
    return 0;
}

typedef Value (*Op)(Value, Value);

static Op op_handlers[] = {
    op_iadd, op_isub,  op_imul, op_idiv, op_irem,  op_ine,  op_ieq,  op_ilt,
    op_ile,  op_igt,   op_ige,  op_fadd, op_fsub,  op_fmul, op_fdiv, op_frem,
    op_fne,  op_feq,   op_flt,  op_fle,  op_fgt,   op_fge,  op_band, op_bor,
    op_bxor, op_dummy, op_land, op_lor,  op_dummy, op_bshl, op_bshr,
};

void superinst_load_const_op(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    uint64_t var_ix = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint64_t const_ix = read_u64(code, vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint8_t res_reg = code[vm->call_frame->ip];
    uint64_t var_val = vm->call_frame->locals[var_ix];
    uint64_t const_val = vm->consts[const_ix];
    vm->regs[res_reg].i =
        op_handlers[code[++vm->call_frame->ip]](var_val, const_val);
#ifdef DEBUG
    printf("var (%ld) - const (%ld) - res: (%ld)\n", var_val, const_val,
           vm->regs[res_reg].i);
#endif
}

void superinst_load_const_op_store(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    uint64_t var_ix = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint64_t const_ix = read_u64(code, vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint64_t var_val = vm->call_frame->locals[var_ix];
    uint64_t const_val = vm->consts[const_ix];
    vm->call_frame->locals[var_ix] =
        op_handlers[code[vm->call_frame->ip]](var_val, const_val);
}

void superinst_jmp_ifnot_cond_op(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    uint64_t jmp_ix = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7;
#ifdef DEBUG
#endif
    uint8_t reg1 = code[++vm->call_frame->ip];
    uint8_t reg2 = code[++vm->call_frame->ip];
    uint8_t res_reg = code[++vm->call_frame->ip];
    uint8_t condop = code[++vm->call_frame->ip];
    vm->regs[res_reg].i =
        op_handlers[condop](vm->regs[reg1].i, vm->regs[reg2].i);
    if (!vm->regs[res_reg].i) {
        vm->call_frame->ip = jmp_ix - 1;
    }
}

static OpcodeHandler superinst_handlers[] = {superinst_load_const_op,
                                             superinst_load_const_op_store,
                                             superinst_jmp_ifnot_cond_op};

void handle_superinstruction(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int inst = code[++vm->call_frame->ip];
    superinst_handlers[inst](vm);
}

void handle_ref(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int var = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int res = code[vm->call_frame->ip];
    Obj *ref = new_obj(vm, OBJ_REF);
    ref->ref.frame = vm->call_frame;
    ref->ref.local_ix = var;
    vm->regs[res].o = ref;
}

void handle_deref(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int var = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int res = code[vm->call_frame->ip];
    Obj *ref = (Obj *)vm->call_frame->locals[var];
    vm->regs[res].i = ref->ref.frame->locals[ref->ref.local_ix];
}

void handle_deref_assign(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int var = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int arg = code[vm->call_frame->ip];
    Obj *ref = (Obj *)vm->call_frame->locals[var];
    ref->ref.frame->locals[ref->ref.local_ix] = vm->regs[arg].i;
}

Obj *clone(VM *vm, Obj *obj, bool gced) {
    Obj *newobj;
    if (gced) {
        newobj = new_obj(vm, obj->type);
    } else {
        newobj = checked_malloc(sizeof(Obj));
        newobj->type = obj->type;
    }
    switch (obj->type) {
    case OBJ_STRING: {
        if (obj->string.constant) {
            newobj->string.data = obj->string.data;
        } else {
            int len = strlen(obj->string.data) + 1;
            newobj->string.data = checked_malloc(len);
            memcpy(newobj->string.data, obj->string.data, len);
        }
        break;
    }
    case OBJ_LIST: {
        newobj->list.length = obj->list.length;
        newobj->list.capacity = obj->list.capacity;
        newobj->list.items = checked_malloc(sizeof(Value) * obj->list.capacity);
        if (obj->list.boxed_items) {
            for (int i = 0; i < obj->list.length; ++i) {
                newobj->list.items[i] =
                    (Value)clone(vm, (Obj *)obj->list.items[i], gced);
            }
        } else {
            memcpy(newobj->list.items, obj->list.items,
                   sizeof(Value) * obj->list.length);
        }
        break;
    }
    default: {
        char msg[1024] = {0};
        sprintf(msg, "Cannot clone object of type %d", obj->type);
        error(vm, msg);
    }
    }
    if (gced) {
        free(obj);
    }
    return newobj;
}

void handle_clone(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int arg = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    bool gced = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[arg].o;
    Obj *newobj = clone(vm, obj, gced);
    vm->regs[res].o = newobj;
}

// Create function pointer table for opcodes
static OpcodeHandler opcode_handlers[] = {
    handle_iadd,     handle_isub,     handle_imul,
    handle_idiv,     handle_irem,     handle_ine,
    handle_ieq,      handle_ilt,      handle_ile,
    handle_igt,      handle_ige,      handle_fadd,
    handle_fsub,     handle_fmul,     handle_fdiv,
    handle_frem,     handle_fne,      handle_feq,
    handle_flt,      handle_fle,      handle_fgt,
    handle_fge,      handle_band,     handle_bor,
    handle_bxor,     handle_bnot,     handle_land,
    handle_lor,      handle_lnot,     handle_bshl,
    handle_bshr,     handle_fneg,     handle_ineg,
    handle_make,     handle_listset,  handle_listget,
    handle_tupleset, handle_tupleget, handle_tuple,
    handle_strget,   handle_mapget,   handle_mapset,
    handle_chansend, handle_chanrecv, handle_jmp,
    handle_jmpifnot, handle_call,     handle_ncall,
    handle_const,    handle_load,     handle_store,
    handle_cast,     handle_move,     handle_return,
    handle_push,     handle_pop,      handle_spawn,
    handle_nop,      handle_ret,      handle_stack_map,
    handle_nop,      handle_nop,      handle_superinstruction,
    handle_corocall, handle_clone};

void run(VM *vm) {
    if (vm->coro_id != MAIN_ID) {
        pthread_mutex_lock(&scheduler->vmq_mu);
        scheduler->coro_running++;
        pthread_mutex_unlock(&scheduler->vmq_mu);
    }

    while (vm->call_frame->ip < vm->call_frame->func->code_length) {
        uint8_t instruction = vm->call_frame->func->code[vm->call_frame->ip];

#ifdef PRINT_OPCODES
        printf("%x %d ", vm->coro_id, vm->call_frame->ip);
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
            printf("%lx ", vm->call_frame->locals[j]);
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

        if (vm->should_yield && vm->is_coro) {
            vm->should_yield = false;
            return;
        }
    }

    vm->halted = true;
}

void *poll_spawn(void *i) {
    uint64_t ix = (uint64_t)(int *)i;
    while (1) {
        // printf("polling %lx\n", ix);
        VM *vm = pop_scheduler();
        // printf("got %p\n", vm);
        vm->coro_id = ix;
        // printf("gonna give control to %p\n", vm);
        run(vm);
        pthread_mutex_lock(&scheduler->vmq_mu);
        scheduler->coro_running--;
        if (vm->halted) {
            TmHeap_destroy(vm->heap);
        } else {
            push_scheduler(vm);
        }
        pthread_cond_signal(&scheduler->vmq_cond);
        pthread_mutex_unlock(&scheduler->vmq_mu);
    }
}

void scheduler_init() {
    scheduler = checked_malloc(sizeof(Scheduler));
    scheduler->coro_queue = new_list();
    scheduler->coro_running = 0;
    pthread_mutex_init(&scheduler->vmq_mu, NULL);
    pthread_cond_init(&scheduler->vmq_cond, NULL);
}

void *main_run(void *vm) {
    run((VM *)vm);
    return NULL;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        return 1;
    }
    const char *filename = argv[1];
    Code *code = read_bytecode(filename);
    // printf("length %ld\n", code->length);
    // dissassemble(code->bytes, code->length);
    // puts("\n\n\n");
    VM *vm = new_vm(code->bytes, code->length);
    scheduler_init();
    pthread_t threads[MAX_THREADS];
    pthread_t mainn;
    vm->coro_id = MAIN_ID;
    pthread_create(&mainn, NULL, main_run, (void *)vm);
    for (uint64_t i = 0; i < MAX_THREADS; ++i) {
        pthread_create(threads + i, NULL, poll_spawn, (int *)i);
    }
    pthread_join(mainn, NULL);
    for (int i = 0; i < MAX_THREADS; ++i) {
        pthread_cancel(threads[i]);
    }
    free(code->bytes);
    free(code);
    free_vm(vm);
}
