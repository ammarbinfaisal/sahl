#include "vm.h"
#include "common.h"
#include "list.h"
#include "obj.h"
#include "rbtree.h"
#include "read.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

VM *new_vm(uint8_t *code, int code_length) {
    VM *vm = malloc(sizeof(struct VM));
    int offset = 0;
    vm->start_func = read_u64(code, offset);
    vm->consts_count = read_u64(code, offset + 8);
    vm->consts = malloc(sizeof(char *) * vm->consts_count);
    offset += 16;

    GCState* gc_state = malloc(sizeof(GCState));
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
            list_append(strings, obj);
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
    LinkedList *node = strings;
    while (node->next != NULL) {
        Obj *obj = node->data;
        obj->next = gc_state->objects;
        gc_state->objects = obj;
        node = node->next;
    }
    list_free(strings);

    // threads
    vm->is_coro = false;
    vm->thread_count = 0;
    vm->threads = NULL;
    vm->coro_to_be_spawned = false;
    vm->coro_id = 0;

    return vm;
}

void free_vm(VM *vm) {
    free(vm->stack);
    free(vm->consts);
    free(vm->gc_state->grayStack);
    free(vm->gc_state);
    free(vm);
}
