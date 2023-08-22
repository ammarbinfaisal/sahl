#include "vm.h"
#include "common.h"
#include "list.h"
#include "obj.h"
#include "rbtree.h"
#include "read.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

VM *new_vm(uint8_t *code, int code_length) {
    VM *vm = malloc(sizeof(struct VM));
    int offset = 0;
    vm->start_func = read_u64(code, offset);
    vm->consts_count = read_u64(code, offset + 8);
    vm->consts = malloc(sizeof(char *) * vm->consts_count);
    offset += 16;

    GCState *gc_state = malloc(sizeof(GCState));
    gc_state->objects = NULL;
    gc_state->grayCount = 0;
    gc_state->grayCapacity = 1024;
    gc_state->grayStack = malloc(sizeof(Obj *) * 1024);
    gc_state->allocated = 0;
    gc_state->nextGC = 1024 * 1024;
    pthread_mutex_init(&gc_state->lock, NULL);
    vm->gc_state = gc_state;

    LinkedList *strings = new_list(); // strings to be added to vm->objects
    for (int i = 0; i < vm->consts_count; ++i) {
        uint8_t ty = code[offset];
        offset++;
        if (ty == 0 || ty == 1) {
            vm->consts[i] = read_u64(code, offset);
            offset += 8;
        }
        if (ty == 2 || ty == 3) {
            vm->consts[i] = code[offset++];
        }
        if (ty == 4) {
            int len = read_u64(code, offset);
            offset += 8;
            char *str = read_string(code, offset, len);
            Obj *obj = malloc(sizeof(Obj));
            obj->type = OBJ_STRING;
            obj->marked = false;
            obj->string.data = str;
            obj->string.constant = true;
            vm->consts[i] = (uint64_t)obj;
            enqueue(strings, obj);
            offset += len;
        }
    }

    int func_count = read_u64(code, offset);
    offset += 8;
    vm->funcs = malloc(sizeof(Func) * func_count);
    vm->funcs_count = func_count;
    for (int i = 0; i < func_count; ++i) {
        uint64_t func_length = read_u64(code, offset);
        offset += 8;
        // int argc = read_u32(code, offset);
        // offset += 4;
        vm->funcs[i].code = code + offset;
        vm->funcs[i].code_length = func_length;
        // vm->funcs[i].args_count = argc;
        offset += func_length;
    }
    vm->stack_size = 0;
    vm->stack = malloc(sizeof(Value) * 1024);
    vm->regs = malloc(sizeof(Value) * 256);
    vm->call_frame = new_call_frame(vm->funcs + vm->start_func, NULL);
    vm->call_frame->locals_count = 0;
    vm->call_frame->locals_capacity = 8;
    vm->call_frame->locals = malloc(sizeof(Value) * 8);

    // garbage collection
    vm->call_frame->stackmap = NULL;

    // add strings to vm->objects
    void *p;
    while ((p = dequeue(strings))) {
        Obj *obj = p;
        obj->next = gc_state->objects;
        gc_state->objects = obj;
    }
    free_linkedlist(strings);

    // threads
    vm->is_coro = false;
    vm->halted = false;
    vm->should_yield = false;

    return vm;
}

VM *coro_vm(VM *curr, int start_func) {
    VM *vm = malloc(sizeof(struct VM));
    vm->start_func = start_func;
    vm->string_count = curr->string_count;
    vm->consts = curr->consts;
    vm->consts_count = curr->consts_count;
    vm->funcs = curr->funcs;
    vm->funcs_count = curr->funcs_count;
    vm->stack_size = 0;
    vm->stack = malloc(sizeof(Value) * 1024);
    vm->regs = malloc(sizeof(Value) * 256);

    // callframe
    vm->call_frame = NULL;

    // stackmap
    if (curr->call_frame->stackmap) {
        vm->call_frame->stackmap = malloc(sizeof(StackMap));
        vm->call_frame->stackmap->len = curr->call_frame->stackmap->len;
        vm->call_frame->stackmap->bits =
            malloc(sizeof(uint8_t) * vm->call_frame->stackmap->len);
        memcpy(vm->call_frame->stackmap->bits, curr->call_frame->stackmap->bits,
               sizeof(uint8_t) * vm->call_frame->stackmap->len);
    }

    // garbage collection
    vm->gc_state = curr->gc_state;

    // threads
    vm->is_coro = true;
    vm->halted = false;
    vm->should_yield = false;

    return vm;
}

void free_vm(VM *vm) {
    free(vm->stack);
    free(vm->consts);
    free(vm->gc_state->grayStack);
    free(vm->gc_state);
    free(vm);
}
