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

static inline void handle_iadd(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i + vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_isub(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i - vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_imul(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i * vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_idiv(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i / vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_irem(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i % vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_ine(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i != vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_ieq(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i == vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_ilt(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i < vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
#ifdef DEBUG
    printf("handle_ilt: %ld < %ld = %d\n", vm->regs[r1].i, vm->regs[r2].i,
           result);
#endif
    vm->regs[res].i = result;
}

static inline void handle_ile(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i <= vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_igt(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i > vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_ige(VM *vm) {
    OP_CODE_R1_R2
    bool result = vm->regs[r1].i >= vm->regs[r2].i;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_fadd(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = arg1 + arg2;
    vm->regs[res].i = *(uint64_t *)&result;
}

static inline void handle_fsub(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = arg1 - arg2;
    vm->regs[res].i = *(int64_t *)&result;
}

static inline void handle_fmul(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = arg1 * arg2;
    vm->regs[res].i = *(int64_t *)&result;
}

static inline void handle_fdiv(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = arg1 / arg2;
    vm->regs[res].i = *(int64_t *)&result;
}

static inline void handle_frem(VM *vm) {
    OP_R1_R2_RES
    DOUBLE_ARG1_ARG2
    double result = fmod(arg1, arg2);
    vm->regs[res].i = *(int64_t *)&result;
}

static inline void handle_fne(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 != arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_feq(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 == arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_flt(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 < arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_fle(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 <= arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_fgt(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 > arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_fge(VM *vm) {
    OP_CODE_R1_R2
    DOUBLE_ARG1_ARG2
    bool result = arg1 >= arg2;
    int res = code[++vm->call_frame->ip];
    vm->regs[res].i = result;
}

static inline void handle_band(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i & vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_bor(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i | vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_bxor(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i ^ vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_bnot(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    int64_t result = ~vm->regs[r1].i;
    vm->regs[res].i = result;
}

static inline void handle_land(VM *vm) {
    OP_R1_R2_RES
    bool result = vm->regs[r1].i && vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_lor(VM *vm) {
    OP_R1_R2_RES
    bool result = vm->regs[r1].i || vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_lnot(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r1 = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    bool result = !vm->regs[r1].i;
    vm->regs[res].i = result;
}

static inline void handle_bshl(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i << vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_bshr(VM *vm) {
    OP_R1_R2_RES
    int64_t result = vm->regs[r1].i >> vm->regs[r2].i;
    vm->regs[res].i = result;
}

static inline void handle_fneg(VM *vm) {
    OP_CODE_R1_R2
    double result = -vm->regs[r1].f;
    vm->regs[r2].f = *(int64_t *)&result;
}

static inline void handle_ineg(VM *vm) {
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

static inline void handle_make(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int res = code[++vm->call_frame->ip];
    int len = code[++vm->call_frame->ip];
    int type = code[++vm->call_frame->ip] - 5;
    // make_fns[type](vm, res, vm->regs[len].i);
    switch (type) {
    case 0:
        make_list(vm, res, vm->regs[len].i);
        break;
    case 1:
        make_map(vm, res, vm->regs[len].i);
        break;
    case 2:
        make_chan(vm, res, vm->regs[len].i);
        break;
    }
}

static inline void handle_listset(VM *vm) {
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

static inline void handle_listget(VM *vm) {
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

static inline void handle_mapset(VM *vm) {
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

static inline void handle_mapget(VM *vm) {
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

static inline void handle_list(VM *vm) {
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

static inline void handle_tuple(VM *vm) {
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

static inline void handle_tupleset(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int tuple = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int reg = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[tuple].o;
    obj->tuple.items[vm->regs[index].i] = vm->regs[reg].i;
}

static inline void handle_tupleget(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int tuple = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[tuple].o;
    vm->regs[res].i = obj->tuple.items[vm->regs[index].i];
}

static inline void handle_chansend(VM *vm) {
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
    if (len == chan->cap && vm->is_coro) {
        vm->should_yield = true;
    }
    pthread_mutex_unlock(&chan->m_mu);
}

static inline void handle_chanrecv(VM *vm) {
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
    if (chan->len == 0 && vm->is_coro) {
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

static inline void handle_strget(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int str = code[++vm->call_frame->ip];
    int index = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[str].o;
    vm->regs[res].i = obj->string.data[index];
}

static inline void handle_jmp(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int offset = read_u64(code, vm->call_frame->ip + 1);
    vm->call_frame->ip = offset - 1;
}

static inline void handle_jmpifnot(VM *vm) {
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

static inline void handle_call(VM *vm) {
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

static inline void handle_corocall(VM *vm) {
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

static inline void handle_ncall(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int func = code[++vm->call_frame->ip];
    // native_fn_t fn = native_functions[func];
    // fn(vm);

    switch (func) {
    case 0:
        native_print_int(vm);
        break;
    case 1:
        native_print_float(vm);
        break;
    case 2:
        native_print_char(vm);
        break;
    case 3:
        native_print_bool(vm);
        break;
    case 4:
        native_print_str(vm);
        break;
    case 5:
        native_append(vm);
        break;
    case 6:
        native_list_len(vm);
        break;
    case 7:
        native_pop(vm);
        break;
    case 8:
        native_make_variant(vm);
        break;
    case 9:
        native_is_variant(vm);
        break;
    case 10:
        native_get_variant(vm);
        break;
    }
}

static inline void handle_const(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int index = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int reg = code[vm->call_frame->ip];
    vm->regs[reg].i = vm->consts[index];
#ifdef DEBUG
    printf("const %ld loaded into reg %d\n", (int64_t)vm->consts[index], reg);
#endif
}

static inline void handle_load(VM *vm) {
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

static inline void handle_store(VM *vm) {
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

static inline void handle_cast(VM *vm) {
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

static inline void handle_move(VM *vm) {
    OP_CODE_R1_R2
    vm->regs[r1].i = vm->regs[r2].i;
#ifdef DEBUG
    printf("reg %d (%ld) moved into reg %d\n", r2, vm->regs[r2].i, r1);
#endif
}

static inline void handle_ret(VM *vm) {
    if (vm->call_frame->prev) {
        CallFrame *curr = vm->call_frame;
        vm->call_frame = vm->call_frame->prev;
        free_call_frame(curr);
    }
}

static inline void handle_return(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r = code[++vm->call_frame->ip];
    vm->regs[0].i = vm->regs[r].i;
    handle_ret(vm);
}

static inline void handle_push(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r = code[++vm->call_frame->ip];
    push(vm, vm->regs[r].i);
}

static inline void handle_pop(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int r = code[++vm->call_frame->ip];
    vm->regs[r].i = pop(vm);
}

static inline void handle_spawn(VM *vm) {
    // this instruction is deprecated
}

static inline void handle_nop(VM *vm) { ; }

static inline void handle_stack_map(VM *vm) {
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
static inline Value op_iadd(Value v1, Value v2) { return v1 + v2; }

static inline Value op_isub(Value v1, Value v2) { return v1 - v2; }

static inline Value op_imul(Value v1, Value v2) { return v1 * v2; }

static inline Value op_idiv(Value v1, Value v2) { return v1 / v2; }

static inline Value op_irem(Value v1, Value v2) { return v1 % v2; }

static inline Value op_ine(Value v1, Value v2) { return v1 != v2; }

static inline Value op_ieq(Value v1, Value v2) { return v1 == v2; }

static inline Value op_ilt(Value v1, Value v2) { return v1 < v2; }

static inline Value op_ile(Value v1, Value v2) { return v1 <= v2; }

static inline Value op_igt(Value v1, Value v2) { return v1 > v2; }

static inline Value op_ige(Value v1, Value v2) { return v1 >= v2; }

static inline Value op_fadd(Value v1, Value v2) {
    double res = (AS_DOUBLE(v1) + AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

static inline Value op_fsub(Value v1, Value v2) {
    double res = (AS_DOUBLE(v1) - AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

static inline Value op_fmul(Value v1, Value v2) {
    double res = (AS_DOUBLE(v1) * AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

static inline Value op_fdiv(Value v1, Value v2) {
    double res = (AS_DOUBLE(v1) / AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

static inline Value op_frem(Value v1, Value v2) {
    double res = fmod(AS_DOUBLE(v1), AS_DOUBLE(v2));
    return *(uint64_t *)&res;
}

static inline Value op_feq(Value v1, Value v2) { return AS_DOUBLE(v1) == AS_DOUBLE(v2); }

static inline Value op_flt(Value v1, Value v2) { return AS_DOUBLE(v1) < AS_DOUBLE(v2); }

static inline Value op_fle(Value v1, Value v2) { return AS_DOUBLE(v1) <= AS_DOUBLE(v2); }

static inline Value op_fgt(Value v1, Value v2) { return AS_DOUBLE(v1) > AS_DOUBLE(v2); }

static inline Value op_fge(Value v1, Value v2) { return AS_DOUBLE(v1) >= AS_DOUBLE(v2); }

static inline Value op_fne(Value v1, Value v2) { return AS_DOUBLE(v1) != AS_DOUBLE(v2); }

static inline Value op_band(Value v1, Value v2) { return v1 & v2; }

static inline Value op_bor(Value v1, Value v2) { return v1 | v2; }

static inline Value op_bxor(Value v1, Value v2) { return v1 ^ v2; }

static inline Value op_land(Value v1, Value v2) { return v1 && v2; }

static inline Value op_lor(Value v1, Value v2) { return v1 || v2; }

static inline Value op_bshl(Value v1, Value v2) { return v1 << v2; }

static inline Value op_bshr(Value v1, Value v2) { return v1 >> v2; }

static inline Value op_dummy(Value _1, Value _2) {
    printf("sigill\n");
    exit(1);
    return 0;
}

static inline Value exec_op(uint8_t op, Value v1, Value v2) {
    switch (op) {
    case OP_IADD:
        return op_iadd(v1, v2);
    case OP_ISUB:
        return op_isub(v1, v2);
    case OP_IMUL:
        return op_imul(v1, v2);
    case OP_IDIV:
        return op_idiv(v1, v2);
    case OP_IREM:
        return op_irem(v1, v2);
    case OP_INE:
        return op_ine(v1, v2);
    case OP_IEQ:
        return op_ieq(v1, v2);
    case OP_ILT:
        return op_ilt(v1, v2);
    case OP_ILE:
        return op_ile(v1, v2);
    case OP_IGT:
        return op_igt(v1, v2);
    case OP_IGE:
        return op_ige(v1, v2);
    case OP_FADD:
        return op_fadd(v1, v2);
    case OP_FSUB:
        return op_fsub(v1, v2);
    case OP_FMUL:
        return op_fmul(v1, v2);
    case OP_FDIV:
        return op_fdiv(v1, v2);
    case OP_FREM:
        return op_frem(v1, v2);
    case OP_FNE:
        return op_fne(v1, v2);
    case OP_FEQ:
        return op_feq(v1, v2);
    case OP_FLT:
        return op_flt(v1, v2);
    case OP_FLE:
        return op_fle(v1, v2);
    case OP_FGT:
        return op_fgt(v1, v2);
    case OP_FGE:
        return op_fge(v1, v2);
    case OP_BAND:
        return op_band(v1, v2);
    case OP_BOR:
        return op_bor(v1, v2);
    case OP_BXOR:
        return op_bxor(v1, v2);
    case OP_LAND:
        return op_land(v1, v2);
    case OP_LOR:
        return op_lor(v1, v2);
    case OP_BSHL:
        return op_bshl(v1, v2);
    case OP_BSHR:
        return op_bshr(v1, v2);
    default:
        return op_dummy(v1, v2); // Default case to handle any invalid op codes
    }
}

static inline void handle_superinstruction(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int inst = code[++vm->call_frame->ip];

    const void *dispatch_table[] = {&&load_const_op, &&load_const_op_store,
                                    &&jmp_ifnot_cond_op, &&load_n, &&const_n};

    goto *dispatch_table[inst];

load_const_op: {
    uint8_t *code = vm->call_frame->func->code;
    uint64_t var_ix = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint64_t const_ix = read_u64(code, vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint8_t res_reg = code[vm->call_frame->ip];
    uint64_t var_val = vm->call_frame->locals[var_ix];
    uint64_t const_val = vm->consts[const_ix];
    vm->regs[res_reg].i =
        exec_op(code[++vm->call_frame->ip], var_val, const_val);
    return;
}

load_const_op_store: {
    uint8_t *code = vm->call_frame->func->code;
    uint64_t var_ix = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint64_t const_ix = read_u64(code, vm->call_frame->ip);
    vm->call_frame->ip += 8;
    uint64_t var_val = vm->call_frame->locals[var_ix];
    uint64_t const_val = vm->consts[const_ix];
    vm->call_frame->locals[var_ix] =
        exec_op(code[vm->call_frame->ip], var_val, const_val);
    return;
}

jmp_ifnot_cond_op: {
    uint8_t *code = vm->call_frame->func->code;
    uint64_t jmp_ix = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7;
    uint8_t reg1 = code[++vm->call_frame->ip];
    uint8_t reg2 = code[++vm->call_frame->ip];
    uint8_t res_reg = code[++vm->call_frame->ip];
    uint8_t condop = code[++vm->call_frame->ip];
    vm->regs[res_reg].i = exec_op(condop, vm->regs[reg1].i, vm->regs[reg2].i);
    if (!vm->regs[res_reg].i) {
        vm->call_frame->ip = jmp_ix - 1;
    }
    return;
}

load_n: {
    // load n values from stack
    uint64_t n = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7;
    for (int i = 0; i < n; ++i) {
        uint64_t var_ix = read_u64(code, ++vm->call_frame->ip);
        vm->call_frame->ip += 8;
        uint8_t reg = code[vm->call_frame->ip];
        vm->regs[reg].i = vm->call_frame->locals[var_ix];
    }
    return;
}

const_n: {
    // load n values from stack
    uint64_t n = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 7;
    for (int i = 0; i < n; ++i) {
        uint64_t const_ix = read_u64(code, ++vm->call_frame->ip);
        vm->call_frame->ip += 8;
        uint8_t reg = code[vm->call_frame->ip];
        vm->regs[reg].i = vm->consts[const_ix];
    }
    return;
}
}

static inline void handle_ref(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int var = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int res = code[vm->call_frame->ip];
    Obj *ref = new_obj(vm, OBJ_REF);
    ref->ref.frame = vm->call_frame;
    ref->ref.local_ix = var;
    vm->regs[res].o = ref;
}

static inline void handle_deref(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int var = read_u64(code, ++vm->call_frame->ip);
    vm->call_frame->ip += 8;
    int res = code[vm->call_frame->ip];
    Obj *ref = (Obj *)vm->call_frame->locals[var];
    vm->regs[res].i = ref->ref.frame->locals[ref->ref.local_ix];
}

static inline void handle_deref_assign(VM *vm) {
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

static inline void handle_clone(VM *vm) {
    uint8_t *code = vm->call_frame->func->code;
    int arg = code[++vm->call_frame->ip];
    int res = code[++vm->call_frame->ip];
    bool gced = code[++vm->call_frame->ip];
    Obj *obj = vm->regs[arg].o;
    Obj *newobj = clone(vm, obj, gced);
    vm->regs[res].o = newobj;
}

void run(VM *vm) {
    if (vm->coro_id != MAIN_ID) {
        pthread_mutex_lock(&scheduler->vmq_mu);
        scheduler->coro_running++;
        pthread_mutex_unlock(&scheduler->vmq_mu);
    }

    static void *dispatch_table[] = {
        &&do_iadd,     &&do_isub,     &&do_imul,
        &&do_idiv,     &&do_irem,     &&do_ine,
        &&do_ieq,      &&do_ilt,      &&do_ile,
        &&do_igt,      &&do_ige,      &&do_fadd,
        &&do_fsub,     &&do_fmul,     &&do_fdiv,
        &&do_frem,     &&do_fne,      &&do_feq,
        &&do_flt,      &&do_fle,      &&do_fgt,
        &&do_fge,      &&do_band,     &&do_bor,
        &&do_bxor,     &&do_bnot,     &&do_land,
        &&do_lor,      &&do_lnot,     &&do_bshl,
        &&do_bshr,     &&do_fneg,     &&do_ineg,
        &&do_make,     &&do_listset,  &&do_listget,
        &&do_tupleset, &&do_tupleget, &&do_tuple,
        &&do_strget,   &&do_mapget,   &&do_mapset,
        &&do_chansend, &&do_chanrecv, &&do_jmp,
        &&do_jmpifnot, &&do_call,     &&do_ncall,
        &&do_const,    &&do_load,     &&do_store,
        &&do_cast,     &&do_move,     &&do_return,
        &&do_push,     &&do_pop,      &&do_spawn,
        &&do_nop,      &&do_ret,      &&do_stack_map,
        &&do_nop,      &&do_nop,      &&do_superinstruction,
        &&do_corocall, &&do_clone,    &&do_halt};

#define maybe_yield                                                            \
    do {                                                                       \
        if (vm->should_yield) {                                                \
            vm->should_yield = false;                                          \
            return;                                                            \
        }                                                                      \
    } while (0)

#define dispatch
    goto *dispatch_table[vm->call_frame->func->code[vm->call_frame->ip]];

#ifdef PRINT_OPCODES
#define print_op(code, ip)                                                     \
    do {                                                                       \
        printf("%x %d ", vm->coro_id, vm->call_frame->ip);                     \
        print_opcode(vm->call_frame->func->code, vm->call_frame->ip);          \
    } while (0)
#else
#define print_op(code, ip)
#endif

#define next                                                                   \
    do {                                                                       \
        vm->call_frame->ip++;                                                  \
        maybe_yield;                                                           \
        print_op(vm->call_frame->func->code, vm->call_frame->ip);              \
        goto *dispatch_table[vm->call_frame->func->code[vm->call_frame->ip]];  \
    } while (0)

    dispatch;
    while (1) {

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

        // opcode_handlers[instruction](vm);

    do_iadd: {
        handle_iadd(vm);
        next;
    }
    do_isub: {
        handle_isub(vm);
        next;
    }
    do_imul: {
        handle_imul(vm);
        next;
    }
    do_idiv: {
        handle_idiv(vm);
        next;
    }
    do_irem: {
        handle_irem(vm);
        next;
    }
    do_ine: {
        handle_ine(vm);
        next;
    }
    do_ieq: {
        handle_ieq(vm);
        next;
    }
    do_ilt: {
        handle_ilt(vm);
        next;
    }
    do_ile: {
        handle_ile(vm);
        next;
    }
    do_igt: {
        handle_igt(vm);
        next;
    }
    do_ige: {
        handle_ige(vm);
        next;
    }
    do_fadd: {
        handle_fadd(vm);
        next;
    }
    do_fsub: {
        handle_fsub(vm);
        next;
    }
    do_fmul: {
        handle_fmul(vm);
        next;
    }
    do_fdiv: {
        handle_fdiv(vm);
        next;
    }
    do_frem: {
        handle_frem(vm);
        next;
    }
    do_fne: {
        handle_fne(vm);
        next;
    }
    do_feq: {
        handle_feq(vm);
        next;
    }
    do_flt: {
        handle_flt(vm);
        next;
    }
    do_fle: {
        handle_fle(vm);
        next;
    }
    do_fgt: {
        handle_fgt(vm);
        next;
    }
    do_fge: {
        handle_fge(vm);
        next;
    }
    do_band: {
        handle_band(vm);
        next;
    }
    do_bor: {
        handle_bor(vm);
        next;
    }
    do_bxor: {
        handle_bxor(vm);
        next;
    }
    do_bnot: {
        handle_bnot(vm);
        next;
    }
    do_land: {
        handle_land(vm);
        next;
    }
    do_lor: {
        handle_lor(vm);
        next;
    }
    do_lnot: {
        handle_lnot(vm);
        next;
    }
    do_bshl: {
        handle_bshl(vm);
        next;
    }
    do_bshr: {
        handle_bshr(vm);
        next;
    }
    do_fneg: {
        handle_fneg(vm);
        next;
    }
    do_ineg: {
        handle_ineg(vm);
        next;
    }
    do_make: {
        handle_make(vm);
        next;
    }
    do_listset: {
        handle_listset(vm);
        next;
    }
    do_listget: {
        handle_listget(vm);
        next;
    }
    do_tupleset: {
        handle_tupleset(vm);
        next;
    }
    do_tupleget: {
        handle_tupleget(vm);
        next;
    }
    do_tuple: {
        handle_tuple(vm);
        next;
    }
    do_strget: {
        handle_strget(vm);
        next;
    }
    do_mapget: {
        handle_mapget(vm);
        next;
    }
    do_mapset: {
        handle_mapset(vm);
        next;
    }
    do_chansend: {
        handle_chansend(vm);
        next;
    }
    do_chanrecv: {
        handle_chanrecv(vm);
        next;
    }
    do_jmp: {
        handle_jmp(vm);
        next;
    }
    do_jmpifnot: {
        handle_jmpifnot(vm);
        next;
    }
    do_call: {
        handle_call(vm);
        next;
    }
    do_ncall: {
        handle_ncall(vm);
        next;
    }
    do_const: {
        handle_const(vm);
        next;
    }
    do_load: {
        handle_load(vm);
        next;
    }
    do_store: {
        handle_store(vm);
        next;
    }
    do_cast: {
        handle_cast(vm);
        next;
    }
    do_move: {
        handle_move(vm);
        next;
    }
    do_return: {
        handle_return(vm);
        next;
    }
    do_push: {
        handle_push(vm);
        next;
    }
    do_pop: {
        handle_pop(vm);
        next;
    }
    do_spawn: {
        handle_spawn(vm);
        next;
    }
    do_nop: {
        handle_nop(vm);
        next;
    }
    do_ret: {
        handle_ret(vm);
        next;
    }
    do_stack_map: {
        handle_stack_map(vm);
        next;
    }
    do_superinstruction: {
        handle_superinstruction(vm);
        next;
    }
    do_corocall: {
        handle_corocall(vm);
        next;
    }
    do_clone: {
        handle_clone(vm);
        next;
    }
    do_halt: { goto end; }
    }

end:
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
    // free_vm(vm);
}
