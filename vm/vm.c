#include "vm.h"
#include "read.h"

VM *new_vm(uint8_t *code, int code_length) {
    VM *vm = malloc(sizeof(struct VM));
    vm->start_func = read_u32(code, 0);
    vm->string_count = read_u32(code, 4);
    vm->strings = malloc(sizeof(char *) * vm->string_count);
    int offset = 8;
    for (int i = 0; i < vm->string_count; ++i) {
        uint32_t strlength = read_u32(code, offset);
        offset += 4;
        vm->strings[i] = read_string(code, offset, strlength);
        offset += strlength;
    }
    int func_count = read_u32(code, offset);
    offset += 4;
    vm->funcs = malloc(sizeof(Func) * func_count);
    vm->funcs_count = func_count;
    for (int i = 0; i < func_count; ++i) {
        uint32_t func_length = read_u32(code, offset);
        offset += 4;
        vm->funcs[i].code = code + offset;
        vm->funcs[i].code_length = func_length;
        offset += func_length;
    }
    vm->stack_size = 0;
    vm->stack = malloc(sizeof(Value) * 1024);
    vm->call_frame = new_call_frame(vm->funcs + vm->start_func, NULL);

    // garbage collection
    vm->objects = NULL;
    vm->grayCount = 0;
    vm->grayCapacity = 1024;
    vm->grayStack = malloc(sizeof(Obj *) * 1024);
    vm->allocated = 0;
    vm->nextGC = 1024 * 1024;

    // threads
    vm->is_coro = false;
    vm->thread_count = 0;
    vm->threads = NULL;
    vm->coro_to_be_spawned = false;

    return vm;
}

VM *coro_vm(VM *curr, int start_func) {
    VM *vm = malloc(sizeof(struct VM));
    vm->start_func = start_func;
    vm->string_count = curr->string_count;
    vm->strings = curr->strings;
    vm->funcs = curr->funcs;
    vm->funcs_count = curr->funcs_count;
    vm->stack_size = 0;
    vm->stack = malloc(sizeof(Value) * 1024);
    vm->call_frame = new_call_frame(curr->funcs + start_func, NULL);

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
    CallFrame *frame = vm->call_frame;
    while (frame) {
        CallFrame *prev = frame->prev;
        free_call_frame(frame);
        frame = prev;
    }
    for (int i = 0; i < vm->string_count; ++i) {
        free(vm->strings[i]);
    }
    free(vm->strings);
    free(vm->grayStack);
    free(vm);
}
