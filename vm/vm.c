#include "vm.h"
#include "common.h"
#include "gc.h"
#include "read.h"
#include "treadmill/darray.h"
#include "treadmill/gc.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static inline Tm_DArray *get_rootset(TmStateHeader *state_header) {
    // Create the dynamic array that we'll return
    Tm_DArray *rootset = Tm_DArray_create(sizeof(TmObjectHeader *), 1024);

    // Typecast to our own State struct
    State *state = (State *)state_header;
    VM *vm = state->vm;

    // Add the VM's stack to the rootset
    CallFrame *frame = vm->call_frame;

    while (frame != NULL) {
        StackMap *map = frame->stackmap;
        if (map) {
            for (int i = 0; i < map->len; i++) {
                for (int j = 0; j < 64; j++) {
                    if (map->bits[i] & (1ull << j)) {
                        TmObjectHeader *obj =
                            (TmObjectHeader *)frame->locals[i * 64 + j];
                        Tm_DArray_push(rootset, obj);
                    }
                }
            }
        }
        frame = frame->prev;
    }

    return rootset;
}

State *State_new() {
    State *state = calloc(1, sizeof(State));
    // Assign the function
    state->gc.rootset = get_rootset;
    return state;
}

void release_my_object(void *obj) {
    Obj *my_obj = (Obj *)obj;

    switch (my_obj->type) {
    case OBJ_STRING:
        if (!my_obj->string.constant) {
            free(my_obj->string.data);
        }
        break;
    case OBJ_LIST:
        if (my_obj->list.boxed_items) {
            for (int i = 0; i < my_obj->list.length; i++) {
                release_my_object((TmObjectHeader *)my_obj->list.items[i]);
            }
        }
        free(my_obj->list.items);
        // TOOD:
        // handle tuples, maps, chans
    }

    free(my_obj);
}

void scan_my_pointers(TmHeap *heap, TmObjectHeader *object,
                      TmCallbackFn callback) {
    Obj *obj = (Obj *)object;

    switch (obj->type) {
    case OBJ_STRING:
        break;
    case OBJ_LIST:
        if (obj->list.boxed_items) {
            for (int i = 0; i < obj->list.length; i++) {
                TmObjectHeader *item = (TmObjectHeader *)obj->list.items[i];
                callback(heap, item);
            }
        }
        break;
        // TOOD:
        // handle tuples, maps, chans
    }
}

VM *new_vm(uint8_t *code, int code_length) {
    VM *vm = checked_malloc(sizeof(struct VM));
    int offset = 0;
    vm->start_func = read_u64(code, offset);
    vm->consts_count = read_u64(code, offset + 8);
    vm->consts = checked_malloc(sizeof(char *) * vm->consts_count);
    offset += 16;

    State *state = State_new();
    state->vm = vm;

    TmHeap *heap = TmHeap_new(
        (TmStateHeader *)state, // your initialized state object
        1024 * 10,              // initial size of the heap
        1024,                   // growth rate in number of cells
        200,                    // scan every 200 allocations
        sizeof(Obj),            // the size of your Objects
        release_my_object,      // a pointer to your release function
        scan_my_pointers        // a pointer to your function to scan pointers
    );

    vm->heap = heap;
    vm->gc_state = state;

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
            Obj *obj = checked_malloc(sizeof(Obj));
            obj->type = OBJ_STRING;
            obj->marked = false;
            obj->string.data = str;
            obj->string.constant = true;
            vm->consts[i] = (uint64_t)obj;
            offset += len;
        }
    }

    int func_count = read_u64(code, offset);
    offset += 8;
    vm->funcs = checked_malloc(sizeof(Func) * func_count);
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
    vm->stack = checked_malloc(sizeof(Value) * 1024);
    vm->regs = checked_malloc(sizeof(Value) * 256);
    vm->call_frame = new_call_frame(vm->funcs + vm->start_func, NULL);
    vm->call_frame->locals_count = 0;
    vm->call_frame->locals_capacity = 8;
    vm->call_frame->locals = checked_malloc(sizeof(Value) * 8);

    // garbage collection
    vm->call_frame->stackmap = NULL;

    // threads
    vm->is_coro = false;
    vm->halted = false;
    vm->should_yield = false;

    return vm;
}

VM *coro_vm(VM *curr, int start_func) {
    VM *vm = checked_malloc(sizeof(struct VM));
    vm->start_func = start_func;
    vm->string_count = curr->string_count;
    vm->consts = curr->consts;
    vm->consts_count = curr->consts_count;
    vm->funcs = curr->funcs;
    vm->funcs_count = curr->funcs_count;
    vm->stack_size = 0;
    vm->stack = checked_malloc(sizeof(Value) * 1024);
    vm->regs = checked_malloc(sizeof(Value) * 256);

    State *state = State_new();
    state->vm = vm;
    vm->heap = TmHeap_new(
        (TmStateHeader *)state, // your initialized state object
        1024,                   // initial size of the heap
        1024,                   // growth rate in number of cells
        200,                    // scan every 200 allocations
        sizeof(Obj),            // the size of your Objects
        release_my_object,      // a pointer to your release function
        scan_my_pointers        // a pointer to your function to scan pointers
    );

    // callframe
    vm->call_frame = new_call_frame(vm->funcs + vm->start_func, NULL);

    // stackmap
    if (curr->call_frame->stackmap) {
        vm->call_frame->stackmap = checked_malloc(sizeof(StackMap));
        vm->call_frame->stackmap->len = curr->call_frame->stackmap->len;
        vm->call_frame->stackmap->bits =
            checked_malloc(sizeof(uint8_t) * vm->call_frame->stackmap->len);
        memcpy(vm->call_frame->stackmap->bits, curr->call_frame->stackmap->bits,
               sizeof(uint8_t) * vm->call_frame->stackmap->len);
    }

    // threads
    vm->is_coro = true;
    vm->halted = false;
    vm->should_yield = false;

    return vm;
}

void free_vm(VM *vm) {
    // TmHeap_destroy(vm->heap); - this is causing a double free
    free(vm->stack);
    free(vm->consts);
    free(vm->regs);
    free(vm);
}
