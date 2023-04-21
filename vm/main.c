#include <math.h>
#include <pthread.h>
#include <stdbool.h>
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
#include "vm.h"
#include "tcp.h"

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
    } else if (IS_NUMBER(value)) {
        sprintf(str, "%lf", AS_FLOAT(value));
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

Obj *new_string(VM *vm, char *chars, int length) {
    Obj *obj = new_obj(vm, OBJ_STRING);
    obj->string.data = chars;
    obj->string.constant = false;
    return obj;
}

Obj *concat_strings(VM *vm, Obj *a, Obj *b) {
    int len1 = strlen(a->string.data);
    int len2 = strlen(b->string.data);
    int length = len1 + len2 + 1;
    char *chars = allocate(vm, length);
    memcpy(chars, a->string.data, len1);
    memcpy(chars + len1, b->string.data, len2);
    chars[length] = '\0';

    return new_string(vm, chars, length);
}

void run(VM *vm); // to be used in handle_call;

// Define opcode handler functions
void handle_add(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);

    if (IS_OBJ(a) && IS_OBJ(b)) {
        push(vm, OBJ_VAL(concat_strings(vm, AS_OBJ(b), AS_OBJ(a))));
    } else {
        push(vm, FLOAT_VAL(AS_FLOAT(a) + AS_FLOAT(b)));
    }
}

void handle_sub(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);
    push(vm, FLOAT_VAL(AS_FLOAT(b) - AS_FLOAT(a)));
}

void handle_mul(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);
    push(vm, FLOAT_VAL(AS_FLOAT(b) * AS_FLOAT(a)));
}

void handle_div(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);
    push(vm, FLOAT_VAL(AS_FLOAT(b) / AS_FLOAT(a)));
}

void handle_mod(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);
    push(vm, FLOAT_VAL(fmod(AS_FLOAT(b), AS_FLOAT(a))));
}

void handle_neg(VM *vm) {
    Value a = pop(vm);
    push(vm, FLOAT_VAL(-AS_FLOAT(a)));
}

void handle_const_u8(VM *vm) {
    uint64_t value = vm->call_frame->func->code[vm->call_frame->ip + 1];
    push(vm, value);
    vm->call_frame->ip += 1;
}

void handle_const_u32(VM *vm) {
    uint64_t value =
        read_u32(vm->call_frame->func->code, vm->call_frame->ip + 1);
    push(vm, value);
    vm->call_frame->ip += 4;
}

void handle_const_u64(VM *vm) {
    uint64_t value =
        read_u64(vm->call_frame->func->code, vm->call_frame->ip + 1);
    push(vm, FLOAT_VAL((double)value));
    vm->call_frame->ip += 8;
}

void handle_true(VM *vm) { push(vm, TRUE_VAL); }

void handle_false(VM *vm) { push(vm, FALSE_VAL); }

void handle_not(VM *vm) {
    Value a = pop(vm);
    push(vm, BOOL_VAL(!AS_BOOL(a)));
}

void handle_and(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(AS_BOOL(a) && AS_BOOL(b)));
}

void handle_or(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(AS_BOOL(a) || AS_BOOL(b)));
}

void handle_equal(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(a == b));
}

void handle_not_equal(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(a != b));
}

void handle_less(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(AS_FLOAT(a) < AS_FLOAT(b)));
}

void handle_less_equal(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(AS_FLOAT(a) <= AS_FLOAT(b)));
}

void handle_greater(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(AS_FLOAT(a) > AS_FLOAT(b)));
}

void handle_greater_equal(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(AS_FLOAT(a) >= AS_FLOAT(b)));
}

void handle_jump(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint64_t ip = read_u32(frame->func->code, frame->ip + 1);
    frame->ip = ip - 1;
}

void handle_jump_if_false(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t ip = read_u32(frame->func->code, frame->ip + 1);
    Value value = pop(vm);
    if (!AS_BOOL(value)) {
        frame->ip = ip - 1;
    } else {
        frame->ip += 4;
    }
}

void handle_pop(VM *vm) { pop(vm); }

void handle_get_local(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t index = read_u32(frame->func->code, frame->ip + 1);
    push(vm, frame->locals[index]);
    frame->ip += 4;
}

void handle_def_local(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t index = read_u32(frame->func->code, frame->ip + 1);
    if (index >= frame->locals_capacity) {
        uint32_t new_capacity = (index + 1) * 2; // Exponential growth
        if (new_capacity <= 16) {
            frame->locals = realloc(frame->locals, sizeof(Value) * 16);
            frame->locals_capacity = 16;
        } else {
            frame->locals =
                realloc(frame->locals, sizeof(Value) * new_capacity);
            frame->locals_capacity = new_capacity;
        }
    }
    frame->locals[index] = pop(vm);
    frame->locals_count = index + 1;
    frame->ip += 4;
}

void handle_list(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t length = read_u32(frame->func->code, frame->ip + 1);
    Obj *obj = new_obj(vm, OBJ_LIST);
    obj->list.length = 0;
    push(vm, OBJ_VAL(obj)); // Prevent GC
    int cap = GROW_CAPACITY(length);
    obj->list.items = allocate(vm, sizeof(Value) * cap);
    obj->list.length = length;
    obj->list.capacity = cap;
    pop(vm); // Remove GC protection
    for (int i = length - 1; i >= 0; --i) {
        obj->list.items[i] = pop(vm);
    }
    obj->list.capacity = cap;
    push(vm, OBJ_VAL(obj));
    frame->ip += 4;
}

void handle_store(VM *vm) {
    Value idx = pop(vm);
    Value arr = pop(vm);
    Value value = pop(vm);
    Obj *obj = AS_OBJ(arr);
    if (obj->type == OBJ_LIST) {
        double index = AS_FLOAT(idx);
        uint64_t i = (uint64_t)trunc(index);
        if (obj->list.length <= index) {
            char msg[100];
            memset(msg, 0, 100);
            sprintf(msg, "Index out of bounds %ld", i);
            error(vm, msg);
        }
        obj->list.items[i] = value;
    } else {
        RBNode *new = new_rb_node(idx);
        new->value = value;
        RBNode *node = rb_insert(obj->map.map, new);
    }
}

void handle_append(VM *vm) {
    Value value = pop(vm);
    Value list = pop(vm);
    Obj *obj = AS_OBJ(list);
    if (obj->list.length >= obj->list.capacity) {
        size_t old_capacity = obj->list.capacity;
        obj->list.capacity = GROW_CAPACITY(old_capacity);
        obj->list.items =
            reallocate(vm, obj->list.items, sizeof(Value) * old_capacity,
                       sizeof(Value) * obj->list.capacity);
    }
    obj->list.items[obj->list.length++] = value;
}

void handle_length(VM *vm) {
    Value value = pop(vm);
    Obj *obj = AS_OBJ(value);
    push(vm, FLOAT_VAL(obj->list.length));
}

void handle_index(VM *vm) {
    Value idx = pop(vm);
    Value value = pop(vm);
    Obj *obj = AS_OBJ(value);
    if (obj->type == OBJ_LIST) {
        double didx = AS_FLOAT(idx);
        uint64_t index = (uint64_t)trunc(didx);
        if (obj->list.length <= index) {
            char msg[100];
            memset(msg, 0, 100);
            sprintf(msg, "Index out of bounds %ld", index);
            error(vm, msg);
        }
        push(vm, obj->list.items[index]);
    } else {
        RBNode *node = rb_search(obj->map.map, idx);
        if (node == NULL) {
            char msg[100];
            memset(msg, 0, 100);
            sprintf(msg, "Key <%s> not found", stringify(idx));
            error(vm, msg);
        }
        push(vm, node->value);
    }
}

void handle_print(VM *vm) {
    Value value = pop(vm);
    print_value(value);
    // putchar('\n');
}

void handle_string(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t stridx = read_u32(frame->func->code, frame->ip + 1);
    char *string = vm->strings[stridx];
    Obj *obj = new_obj(vm, OBJ_STRING);
    obj->string.data = string;
    obj->string.constant = true;
    push(vm, OBJ_VAL(obj));
    frame->ip += 4;
}

void handle_make_tuple(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t len = read_u32(frame->func->code, frame->ip + 1);
    Obj *obj = new_obj(vm, OBJ_TUPLE);
    push(vm, OBJ_VAL(obj)); // Prevent GC
    obj->tuple.items = allocate(vm, sizeof(Value) * len);
    obj->tuple.length = len;
    pop(vm);
    for (int i = len - 1; i >= 0; --i) {
        obj->tuple.items[i] = pop(vm);
    }
    push(vm, OBJ_VAL(obj));
    frame->ip += 4;
}

void handle_make_list(VM *vm) {
    Value def = pop(vm);
    double dlen = AS_FLOAT(pop(vm));
    uint64_t len = (uint64_t)dlen;
    Obj *obj = new_obj(vm, OBJ_LIST);
    push(vm, OBJ_VAL(obj)); // premptive GC prevention
    size_t cap = GROW_CAPACITY(len);
    obj->list.items = allocate(vm, sizeof(Value) * cap);
    obj->list.length = len;
    obj->list.capacity = cap;
    for (int i = 0; i < len; ++i) {
        obj->list.items[i] = def;
    }
}

void handle_local(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t index = read_u32(frame->func->code, frame->ip + 1);
    push(vm, frame->locals[index]);
    frame->ip += 4;
}

void handle_assign(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t index = read_u32(frame->func->code, frame->ip + 1);
    Value val = pop(vm);
    frame->locals[index] = val;
    frame->ip += 4;
}

void handle_return(VM *vm) {
    if (vm->is_coro) {
        pthread_exit(NULL);
#ifdef DEBUG
        printf("exiting a thread\n");
#endif
        return;
    }

    if (vm->call_frame->depth == 0) {
        return;
    }

    CallFrame *call_frame = vm->call_frame->prev;
    free(vm->call_frame);
    vm->call_frame = call_frame;
}

void *spawn(void *vm) {
    VM *nvm = (VM *)vm;
    run(nvm);
    return NULL;
}

void handle_call(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t depth = frame->depth + 1;
    if (depth == MAX_CALL_DEPTH) {
        error(vm, "Maximum call depth exceeded");
    }

    uint8_t *code = frame->func->code;
    uint32_t ip = frame->ip;
    uint32_t funcidx = read_u32(code, ip + 1);
    uint32_t argc = read_u32(code, ip + 5);

    VM *nvm;
    CallFrame *newframe;
    if (vm->coro_to_be_spawned) {
        nvm = coro_vm(vm, funcidx);
        newframe = nvm->call_frame;
    } else {
        nvm = vm;
        newframe = new_call_frame(vm->funcs + funcidx, frame);
    }

    Value *args = allocate(nvm, sizeof(Value) * argc);
    for (int i = argc - 1; i >= 0; --i) {
        Value val = pop(vm);
        args[i] = val;
    }

    newframe->locals_capacity = argc;
    newframe->locals_count = argc;
    newframe->locals = args;
    newframe->depth = depth + 1;
    newframe->func = vm->funcs + funcidx;
    newframe->ip = -1;
    nvm->call_frame = newframe;
    frame->ip += 8;

    if (vm->coro_to_be_spawned) {
        vm->coro_to_be_spawned = false;
        vm->thread_count++;
        newframe->ip++; // should be 0
        nvm->coro_id = coro_count++;
        if (vm->thread_count == MAX_COROS) {
            int i = 0;
            while (i < vm->thread_count && vm->coro_done[i]) {
                i++;
            }
            pthread_join(vm->threads[i], NULL);
            vm->coro_done[i] = true;
            vm->thread_count--;
        }
        pthread_t thread = (pthread_t)malloc(sizeof(pthread_t));
        pthread_create(&thread, NULL, spawn, nvm);
        vm->threads = realloc(vm->threads, vm->thread_count * sizeof(pthread_t));
        vm->coro_done = realloc(vm->coro_done, vm->thread_count * sizeof(bool));
        vm->threads[vm->thread_count - 1] = thread;
        vm->coro_done[vm->thread_count - 1] = false;
    }
}

void handle_const_64(VM *vm) {
    uint64_t val = read_u64(vm->call_frame->func->code, vm->call_frame->ip + 1);
    push(vm, FLOAT_VAL(val));
    vm->call_frame->ip += 8;
}

void handle_const_32(VM *vm) {
    uint32_t val = read_u32(vm->call_frame->func->code, vm->call_frame->ip + 1);
    push(vm, val);
    vm->call_frame->ip += 4;
}

void handle_const_8(VM *vm) {
    uint8_t val = vm->call_frame->func->code[vm->call_frame->ip + 1];
    push(vm, val);
    vm->call_frame->ip += 2;
}

void handle_const_double(VM *vm) {
    double val =
        read_double(vm->call_frame->func->code, vm->call_frame->ip + 1);
    push(vm, FLOAT_VAL(val));
    vm->call_frame->ip += 8;
}

// ------------------ NATIVE FUNCTIONS ------------------

#define PRE_NATIVE                                                             \
    CallFrame *frame = vm->call_frame;                                         \
    uint32_t funcidx = read_u32(frame->func->code, frame->ip + 1);             \
    uint32_t argc = read_u32(frame->func->code, frame->ip + 5);                \
    Value *args = allocate(vm, sizeof(Value) * argc);                          \
    for (int i = argc - 1; i >= 0; --i) {                                      \
        Value val = pop(vm);                                                   \
        args[i] = val;                                                         \
    }

typedef void (*native_fn_t)(VM *vm);

void native_clear_screen(VM *vm) { printf("\033[2J\033[1;1H"); }

void native_rand(VM *vm) {
    PRE_NATIVE
    int r = rand() % (int)AS_FLOAT(args[0]) + AS_FLOAT(args[1]);
    push(vm, FLOAT_VAL((double)r));
}

void native_sleep(VM *vm) {
    PRE_NATIVE
    sleep(args[0]);
}

void native_randf(VM *vm) {
    PRE_NATIVE
    push(vm, FLOAT_VAL((float)rand() / (float)RAND_MAX));
}

void native_exp(VM *vm) {
    PRE_NATIVE
    push(vm, FLOAT_VAL(exp(AS_FLOAT(args[0]))));
}

void native_pow(VM *vm) {
    PRE_NATIVE
    push(vm, FLOAT_VAL(pow(AS_FLOAT(args[0]), AS_FLOAT(args[1]))));
}

void native_exit(VM *vm) {
    PRE_NATIVE
    free_vm(vm);
    exit(args[0]);
}

void native_print(VM *vm) {
    PRE_NATIVE
    // char str[1024 * 4];
    // memset(str, 0, 1024 * 4);
    // int len = 0;
    // for (int i = 0; i < argc; ++i) {
    //     char *s = stringify(args[i]);
    //     memcpy(str + len, s, strlen(s));
    //     len += strlen(s);
    //     free(s);
    //     if (len > 1024 * 3.5) {
    //         str[len] = '.';
    //         str[len + 1] = '.';
    //         str[len + 2] = '.';
    //         str[len + 3] = '\0';
    //         break;
    //     }
    // }
    // printf("%s", str);
    for (int i = 0; i < argc; ++i) {
        print_value(args[i]);
    }
}

void native_tanh(VM *vm) {
    PRE_NATIVE
    push(vm, FLOAT_VAL(tanh(AS_FLOAT(args[0]))));
}

void native_log(VM *vm) {
    PRE_NATIVE
    push(vm, FLOAT_VAL(log(AS_FLOAT(args[0]))));
}

void native_tcp_server(VM *vm) {
    PRE_NATIVE
    double portf = AS_FLOAT(args[0]);
    int port = (int)trunc(portf);
    Chan *chan = AS_OBJ(args[1])->channel.chan;
    TcpServer *server = malloc(sizeof(TcpServer));
    server->port = port;
    server->chan = chan;
    server->vm = vm;
    pthread_t thread;
    pthread_create(&thread, NULL, tcp_server_thread, (void *)server);
    APPEND_THREAD(thread)
}

void native_close_chan(VM *vm) {
    PRE_NATIVE
    Chan *chan = AS_OBJ(args[0])->channel.chan;
    close_chan(chan);
}

void native_is_open_chan(VM *vm) {
    PRE_NATIVE
    Chan *chan = AS_OBJ(args[0])->channel.chan;
    push(vm, BOOL_VAL(!chan->closed));
}

static native_fn_t native_functions[] = {
    native_clear_screen, native_rand, native_sleep, native_randf, native_exp,
    native_pow,          native_exit, native_print, native_tanh,  native_log,
    native_tcp_server,   native_close_chan, native_is_open_chan
};

void handle_native_call(VM *vm) {
    CallFrame *frame = vm->call_frame;
    uint32_t funcidx = read_u32(frame->func->code, frame->ip + 1);
    native_functions[funcidx](vm);
    frame->ip += 8;
}

void handle_make_chan(VM *vm) {
    Obj *obj = new_obj(vm, OBJ_CHAN);
    obj->channel.chan = new_chan(128);
    push(vm, OBJ_VAL(obj));
}

void handle_chan_read(VM *vm) {
    Obj *obj = AS_OBJ(pop(vm));
    if (obj->type != OBJ_CHAN) {
        error(vm, "Expected channel");
    }
    Value val;
    chan_read(obj->channel.chan, &val);
    push(vm, val);
}

void handle_chan_write(VM *vm) {
    Obj *obj = AS_OBJ(pop(vm));
    Value val = pop(vm);
    if (obj->type != OBJ_CHAN) {
        error(vm, "Expected channel");
    }
    chan_write(obj->channel.chan, val);
}

void handle_spawn(VM *vm) { vm->coro_to_be_spawned = true; }

void handle_make_map(VM *vm) {
    Obj *obj = new_obj(vm, OBJ_MAP);
    obj->map.map = new_rb_node(0);
    push(vm, OBJ_VAL(obj));
}

// Create function pointer table for opcodes
static OpcodeHandler opcode_handlers[NUM_OPCODES] = {
    handle_add,           handle_sub,         handle_mul,
    handle_div,           handle_mod,         handle_neg,
    handle_not,           handle_and,         handle_or,
    handle_equal,         handle_not_equal,   handle_less,
    handle_less_equal,    handle_greater,     handle_greater_equal,
    handle_true,          handle_false,       handle_jump,
    handle_jump_if_false, handle_store,       handle_index,
    handle_append,        handle_length,      handle_list,
    handle_const_64,      handle_const_32,    handle_const_8,
    handle_string,        handle_def_local,   handle_get_local,
    handle_assign,        handle_call,        handle_return,
    handle_print,         handle_pop,         handle_make_list,
    handle_make_tuple,    handle_native_call, handle_const_double,
    handle_make_chan,     handle_chan_read,   handle_chan_write,
    handle_spawn,         handle_make_map,
};

void run(VM *vm) {
    while (vm->call_frame->ip < vm->call_frame->func->code_length) {
        uint8_t instruction = vm->call_frame->func->code[vm->call_frame->ip];

#ifdef PRINT_OPCODES
        printf("%d ", vm->coro_id);
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

        ++vm->call_frame->ip;
    }

    for (int i = 0; i < vm->thread_count; ++i) {
        if (!vm->coro_done[i]) pthread_join(vm->threads[i], NULL);
    }

    // clear call frame
    vm->call_frame->locals_count = 0;
    collect_garbage(vm);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        return 1;
    }
    const char *filename = argv[1];
    Code *code = read_bytecode(filename);
    printf("length %ld\n", code->length);
    dissassemble(code->bytes, code->length);
    puts("\n\n\n");
    VM *vm = new_vm(code->bytes, code->length);
    run(vm);
    free(code->bytes);
    free(code);
    free_vm(vm);
}
