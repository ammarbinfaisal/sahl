#include "vm.h"
#include "common.h"
#include "obj.h"
#include "rbtree.h"
#include "list.h"
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

    LinkedList* strings = new_list(); // strings to be added to vm->objects
    LinkedList* strings_tail = strings;
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
            vm->objects = obj;
            obj->marked = false;
            obj->string.data = str;
            obj->string.constant = true;
            vm->consts[i] = (uint64_t)obj;
            list_append(&strings_tail, obj);
            strings_tail = strings_tail->next;
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
    vm->objects = NULL;
    vm->grayCount = 0;
    vm->grayCapacity = 1024;
    vm->grayStack = malloc(sizeof(Obj *) * 1024);
    vm->allocated = 0;
    vm->nextGC = 1024 * 1024;

    // add strings to vm->objects
    LinkedList *node = strings;
    while (node != NULL) {
        Obj* obj = node->data;
        obj->next = vm->objects;
        vm->objects = obj;
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

VM *coro_vm(VM *curr, int start_func) {
    VM *vm = malloc(sizeof(struct VM));
    vm->start_func = start_func;
    vm->string_count = curr->string_count;
    vm->consts = curr->consts;
    vm->funcs = curr->funcs;
    vm->funcs_count = curr->funcs_count;
    vm->stack_size = 0;
    vm->stack = malloc(sizeof(Value) * 1024);
    vm->regs = malloc(sizeof(Value) * 256);
    vm->call_frame = new_call_frame(curr->funcs + start_func, NULL);
    vm->call_frame->locals_count = 0;
    vm->call_frame->locals_capacity = 8;
    vm->call_frame->locals = malloc(sizeof(Value) * 8); 

    // garbage collection
    vm->objects = NULL;
    vm->grayCount = 0;
    vm->grayCapacity = 1024;
    vm->grayStack = malloc(sizeof(Obj *) * 1024);
    vm->allocated = 0;
    vm->nextGC = 1024 * 1024;

    // threads
    vm->is_coro = true;
    vm->thread_count = 0;
    vm->threads = NULL;

    return vm;
}

void free_vm(VM *vm) {
    for (int i = 0; i < vm->stack_size; ++i) {
        free_value(vm->stack[i]);
    }
    free(vm->stack);
    free(vm->consts);
    free(vm->grayStack);
    free(vm);
}
