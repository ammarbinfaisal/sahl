#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ADD 0
#define SUB 1
#define MUL 2
#define DIV 3
#define MOD 4
#define NEG 5
#define NOT 6
#define AND 7
#define OR 8
#define EQUAL 9
#define NOT_EQUAL 10
#define LESS 11
#define LESS_EQUAL 12
#define GREATER 13
#define GREATER_EQUAL 14
#define TRUE 15
#define FALSE 16
#define JUMP 17
#define JUMP_IF_FALSE 18
#define STORE 19
#define INDEX 20
#define APPEND 21
#define LENGTH 22
#define LIST 23
#define CONST_U64 24
#define CONST_U32 25
#define CONST_U8 26
#define STRING 27
#define DEF_LOCAL 28
#define GET_LOCAL 29
#define ASSIGN 30
#define CALL 31
#define RETURN 32
#define PRINT 33
#define POP 34
#define MAKE_LIST 35
#define MAKE_TUPLE 36
#define NUM_OPCODES 37

#define MAX_STACK 1024
#define MAX_CALL_DEPTH 1024

// #define PRINT_OPCODES
// #define PRINT_STACK
// #define PRINT_LOCALS
// #define DEBUG
#define UNSAFE

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN ((uint64_t)0x7ffc000000000000)

#define TAG_FALSE 2 // 10.
#define TAG_TRUE 3  // 11.

#define GC_HEAP_GROW_FACTOR 2
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*1.5)

typedef uint64_t Value;

enum ObjType {
    OBJ_STRING,
    OBJ_LIST,
    OBJ_TUPLE,
};

typedef enum ObjType ObjType;

struct Func {
    uint8_t *code;
    int code_length;
};

typedef struct Func Func;

struct CallFrame {
    uint32_t ip;
    Func *func;
    int locals_count;
    int locals_capacity;
    Value *locals;
    struct CallFrame *prev;
    int depth;
};

typedef struct CallFrame CallFrame;

struct Obj {
    bool marked;
    ObjType type;
    struct Obj *next;
    union {
        struct {
            char *data;
            bool constant;
        } string;
        struct {
            uint64_t capacity;
            uint64_t length;
            Value *items;
        } list;
        struct {
            uint64_t length;
            Value *items;
        } tuple;
        struct {
            CallFrame *frame;
        } closure;
    };
};

typedef struct Obj Obj;

#define IS_BOOL(value) (((value) | 1) == TRUE_VAL)
#define IS_NUMBER(value) (((value)&QNAN) != QNAN)
#define IS_OBJ(value) (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value) ((value) == TRUE_VAL)
#define AS_OBJ(value) ((Obj *)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

#define BOOL_VAL(b) ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define OBJ_VAL(obj) (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

struct Code {
    uint32_t start_ip;
    uint8_t *bytes;
    long length;
};

typedef struct Code Code;

Code *read_bytecode(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (file == NULL) {
        printf("Error: Could not open file %s", filename);
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *buffer = malloc(size);
    fread(buffer, 1, size, file);
    fclose(file);
    Code *code = malloc(sizeof(Code));
    code->bytes = buffer;
    code->length = size;
    return code;
}

uint32_t read_u32(uint8_t *code, int idx) {
    union {
        uint8_t *restrict u8;
        uint32_t *restrict u32;
    } conv = {code + idx};
    return *conv.u32;
}

uint64_t read_u64(uint8_t *code, int idx) {
    union {
        uint8_t *restrict u8;
        uint32_t *restrict u64;
    } conv = {code + idx};
    return *conv.u64;
}

char *read_string(uint8_t *code, int idx, int len) {
    char *str = malloc(len + 1);
    memcpy(str, code + idx, len);
    str[len] = '\0';
    return str;
}

int print_opcode(uint8_t *code, int i) {
    switch (code[i]) {
    case ADD:
        printf("Add\n");
        break;
    case SUB:
        printf("Sub\n");
        break;
    case MUL:
        printf("Mul\n");
        break;
    case DIV:
        printf("Div\n");
        break;
    case MOD:
        printf("Mod\n");
        break;
    case NEG:
        printf("Neg\n");
        break;
    case NOT:
        printf("Not\n");
        break;
    case AND:
        printf("And\n");
        break;
    case OR:
        printf("Or\n");
        break;
    case EQUAL:
        printf("Equal\n");
        break;
    case NOT_EQUAL:
        printf("NotEqual\n");
        break;
    case LESS:
        printf("Less\n");
        break;
    case LESS_EQUAL:
        printf("LessEqual\n");
        break;
    case GREATER:
        printf("Greater\n");
        break;
    case GREATER_EQUAL:
        printf("GreaterEqual\n");
        break;
    case TRUE:
        printf("True\n");
        break;
    case FALSE:
        printf("False\n");
        break;
    case JUMP:
        // u64 code
        printf("Jump %u\n", read_u32(code, i + 1));
        i += 4;
        break;
    case JUMP_IF_FALSE:
        // u64 code
        printf("JumpIfFalse %u\n", read_u32(code, i + 1));
        i += 4;
        break;
    case STORE:
        // u8 index
        printf("Store");
        break;
    case INDEX:
        printf("Index\n");
        break;
    case CONST_U32:
        // u32 code
        printf("ConstU32 %u\n", read_u32(code, i + 1));
        i += 4;
        break;
    case CONST_U64:
        // u64 code
        printf("ConstU64 %lu\n", read_u64(code, i + 1));
        i += 8;
        break;
    case CONST_U8:
        // u8 code
        printf("ConstU8 %u\n", code[i + 1]);
        i += 1;
        break;
    case LIST:
        // u32 length
        printf("List %u\n", read_u32(code, i + 1));
        i += 4;
        break;
    case MAKE_LIST:
        // u32 length
        printf("MakeList\n");
        i += 1;
        break;
    case STRING: {
        // u32 length
        uint32_t stridx = read_u32(code, i + 1);
        i += 5;
        printf("string at index %d\n", stridx);
        break;
    }
    case APPEND:
        printf("Append\n");
        break;
    case DEF_LOCAL:
        // u32 index
        printf("DefLocal %u\n", read_u32(code, i + 1));
        i += 4;
        break;
    case GET_LOCAL:
        // u32 index
        printf("GetLocal %u\n", read_u32(code, i + 1));
        i += 4;
        break;
    case ASSIGN:
        // u32 index
        printf("Assign %u\n", read_u32(code, i + 1));
        i += 4;
        break;
    case LENGTH:
        printf("Length\n");
        break;
    case CALL:
        // u32 function index, u32 arg count
        printf("Call \t fn: %u \t arg count: %u \n", read_u32(code, i + 1),
               read_u32(code, i + 5));
        i += 8;
        break;
    case RETURN:
        printf("Return\n");
        break;
    case PRINT:
        printf("Print\n");
        break;
    case POP:
        printf("Pop\n");
        break;
    default:
        printf("Unknown opcode %u\n", code[i]);
        break;
    }
    return i;
}

void dissassemble(uint8_t *code, int length) {
    int start_ = read_u32(code, 0);
    puts("strings:");
    int i = 4;
    int strings_count = read_u32(code, i);
    i += 4;
    while (strings_count-- && i < length) {
        uint32_t len = read_u32(code, i);
        i += 4;
        char *str = read_string(code, i, len);
        printf("\t%d ", len);
        printf("%s\n", str);
        i += len;
        free(str);
    }
    // read func count
    // read func length
    // then read those bytes and print opcode
    int func_count = read_u32(code, i);
    i += 4;
    for (int j = 0; j < func_count; j++) {
        int func_length = read_u32(code, i);
        i += 4;
        printf("func %d - length %d\n", j, func_length);
        int start_ = i;
        int end = i + func_length;
        while (i < end) {
            printf("%4d ", i - start_);
            i = print_opcode(code, i);
            ++i;
        }
    }
}

struct VM {
    Value *stack;
    int stack_size;
    Func *funcs;
    int funcs_count;
    int string_count;
    char **strings;
    CallFrame *call_frame;
    int start_func;

    // garbage collection
    Obj *objects;
    int grayCount;
    int grayCapacity;
    Obj **grayStack;
    uint64_t allocated;
    uint64_t nextGC;
};

typedef struct VM VM;

void free_value(Value value);

CallFrame *new_call_frame(Func *func, CallFrame *prev) {
    CallFrame *frame = malloc(sizeof(CallFrame));
    frame->ip = 0;
    frame->func = func;
    frame->locals_count = 0;
    frame->locals_capacity = 0;
    frame->locals = NULL;
    frame->prev = prev;
    frame->depth = prev ? prev->depth + 1 : 0;
    return frame;
}

void free_call_frame(CallFrame *frame) {
    free(frame->locals);
    free(frame);
}

VM *new_vm(uint8_t *code, int code_length) {
    VM *vm = malloc(sizeof(struct VM));
    vm->start_func = read_u32(code, 0);
    vm->string_count = read_u32(code, 4);
    vm->strings = malloc(sizeof(char *) * vm->string_count);
    int offset = 8;
    for (int i = 0; i < vm->string_count; ++i) {
        uint32_t strlength = read_u32(code, offset);
        printf("reading string of %d\n", strlength);
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

void print_value(Value value) {
    if (IS_BOOL(value)) {
        printf("%s", AS_BOOL(value) ? "true" : "false");
    } else if (IS_NUMBER(value)) {
        printf("%ld", value);
    } else if (IS_OBJ(value)) {
        Obj *obj = AS_OBJ(value);

#ifdef DEBUG
        printf("%p ", obj);
#endif

        if (obj->type == OBJ_STRING) {
            printf("%s", obj->string.data);
        } else if (obj->type == OBJ_LIST) {
            printf("[");
            for (int i = 0; i < obj->list.length; i++) {
                print_value(obj->list.items[i]);
                printf(", ");
            }
            printf("]");
        } else if (obj->type == OBJ_TUPLE) {
            printf("(");
            for (int i = 0; i < obj->tuple.length; i++) {
                print_value(obj->tuple.items[i]);
                if (i != obj->tuple.length - 1) printf(", ");
            }
            printf(")");
        }
    }
}

void free_obj(Obj *obj) {
#ifdef DEBUG
    printf("freeing %p\n\n", obj);
#endif

    if (obj->type == OBJ_LIST) {
#ifdef DEBUG
        printf("freeing list items %p\n", obj->list.items);
#endif
        free(obj->list.items);
    } else if (obj->type == OBJ_TUPLE) {
#ifdef DEBUG
        printf("freeing tuple items %p\n", obj->tuple.items);
#endif
        free(obj->tuple.items);
    } else if (obj->type == OBJ_STRING && !obj->string.constant) {
#ifdef DEBUG
        printf("freeing string chars %p\n", obj->string.data);
#endif
        free(obj->string.data);
    }

    free(obj);
}

void free_value(Value value) {
    if (IS_OBJ(value)) {
        free_obj(AS_OBJ(value));
    }
}

void mark_obj(VM *vm, Obj *obj) {
#ifdef DEBUG
    printf("marking %p\n", obj);
    print_value(OBJ_VAL(obj));
#endif

    if (obj->marked) return;
    obj->marked = true;

    if (vm->grayCapacity < vm->grayCount + 1) {
        vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
        vm->grayStack =
            (Obj **)realloc(vm->grayStack, sizeof(Obj *) * vm->grayCapacity);
    }

    vm->grayStack[vm->grayCount++] = obj;
}

void mark_value(VM *vm, Value value) {
    if (IS_OBJ(value)) {
        mark_obj(vm, AS_OBJ(value));
    }
}

static void blacken_object(VM *vm, Obj *obj) {
    switch (obj->type) {
    case OBJ_STRING: {
        break;
    }
    case OBJ_LIST: {
        for (int i = 0; i < obj->list.length; i++) {
            mark_value(vm, obj->list.items[i]);
        }
        break;
    }
    case OBJ_TUPLE: {
        for (int i = 0; i < obj->tuple.length; i++) {
            mark_value(vm, obj->tuple.items[i]);
        }
        break;
    }
    }
}

static void trace_references(VM *vm) {
    while (vm->grayCount > 0) {
        Obj *object = vm->grayStack[--vm->grayCount];
        blacken_object(vm, object);
    }
}

static void sweep(VM *vm) {
    Obj *previous = NULL;
    Obj *object = vm->objects;
    while (object != NULL) {
        if (object->marked) {
            object->marked = false;
#ifdef DEBUG
            printf("unmarking %p\n", object);
#endif
            previous = object;
            object = object->next;
        } else {
            Obj *unreached = object;
            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm->objects = object;
            }

            free_obj(unreached);
        }
    }
}

void mark_roots(VM *vm) {
    for (Value *slot = vm->stack; slot < vm->stack + vm->stack_size; slot++) {
        mark_value(vm, *slot);
    }

    // current call frame
    CallFrame *frame = vm->call_frame;
    while (frame != NULL) {
        for (int i = 0; i < frame->locals_count; i++) {
            mark_value(vm, frame->locals[i]);
        }
        frame = frame->prev;
    }
}

void collect_garbage(VM *vm) {
    // printf("Collecting garbage...
    mark_roots(vm);
    trace_references(vm);
    sweep(vm);
    vm->nextGC = vm->allocated * GC_HEAP_GROW_FACTOR;
}

void *allocate(VM *vm, size_t size) {
    vm->allocated += size;
    void *ptr = malloc(size);

#ifdef DEBUG
    printf("allocated %p of size %ld\n", ptr, size);
#endif

    if (vm->allocated > vm->nextGC) {
        collect_garbage(vm);
    }

    return ptr;
}

void *reallocate(VM *vm, void *ptr, size_t oldSize, size_t newSize) {
    vm->allocated += newSize - oldSize;
    void *new_ptr = realloc(ptr, newSize);

#ifdef DEBUG
    printf("reallocating %p from %ld to %ld\n", ptr, oldSize, newSize);
#endif

    if (vm->allocated > vm->nextGC) {
        collect_garbage(vm);
    }

    return new_ptr;
}

Obj *new_obj(VM *vm, ObjType type) {
    Obj *obj = allocate(vm, sizeof(Obj));
    obj->type = type;
    obj->next = vm->objects;
    vm->objects = obj;
    obj->marked = false;
    return obj;
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

// Define opcode handler functions
void handle_add(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);

    if (IS_OBJ(a) && IS_OBJ(b)) {
        push(vm, OBJ_VAL(concat_strings(vm, AS_OBJ(b), AS_OBJ(a))));
    } else {
        push(vm, a + b);
    }
}

void handle_sub(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);
    push(vm, b - a);
}

void handle_mul(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);
    push(vm, b * a);
}

void handle_div(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);
    push(vm, b / a);
}

void handle_mod(VM *vm) {
    Value a = pop(vm);
    Value b = pop(vm);
    push(vm, b % a);
}

void handle_neg(VM *vm) {
    Value a = pop(vm);
    push(vm, -a);
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
    push(vm, value);
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
    push(vm, BOOL_VAL(a < b));
}

void handle_less_equal(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(a <= b));
}

void handle_greater(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(a > b));
}

void handle_greater_equal(VM *vm) {
    Value b = pop(vm);
    Value a = pop(vm);
    push(vm, BOOL_VAL(a >= b));
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
    Value index = pop(vm);
    Value arr = pop(vm);
    Value value = pop(vm);
    Obj *obj = AS_OBJ(arr);
    if (obj->list.length <= index) {
        char msg[100];
        memset(msg, 0, 100);
        sprintf(msg, "Index out of bounds %ld", index);
        error(vm, msg);
    }
    obj->list.items[index] = value;
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
    push(vm, obj->list.length);
}

void handle_index(VM *vm) {
    Value index = pop(vm);
    Value value = pop(vm);
    Obj *obj = AS_OBJ(value);
    if (obj->list.length <= index) {
        char msg[100];
        memset(msg, 0, 100);
        sprintf(msg, "Index out of bounds %ld", index);
        error(vm, msg);
    }
    push(vm, obj->list.items[index]);
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
    Value len = pop(vm);
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
    if (vm->call_frame->depth == 0) {
        return;
    }

    CallFrame *call_frame = vm->call_frame->prev;
    free(vm->call_frame);
    vm->call_frame = call_frame;
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

    Value *args = allocate(vm, sizeof(Value) * argc);
    for (int i = argc - 1; i >= 0; --i) {
        Value val = pop(vm);
        args[i] = val;
    }

    CallFrame *curr = vm->call_frame;
    curr->ip += 8;
    CallFrame *newframe = new_call_frame(vm->funcs + funcidx, curr);
    newframe->locals_capacity = argc;
    newframe->locals_count = argc;
    newframe->locals = args;
    newframe->depth = curr->depth + 1;
    newframe->func = vm->funcs + funcidx;
    vm->call_frame = newframe;
    vm->call_frame->ip = -1;
}

void handle_const_64(VM *vm) {
    uint64_t val = read_u64(vm->call_frame->func->code, vm->call_frame->ip + 1);
    push(vm, val);
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

// Create function pointer table for opcodes
static OpcodeHandler opcode_handlers[NUM_OPCODES] = {
    handle_add,        handle_sub,       handle_mul,           handle_div,
    handle_mod,        handle_neg,       handle_not,           handle_and,
    handle_or,         handle_equal,     handle_not_equal,     handle_less,
    handle_less_equal, handle_greater,   handle_greater_equal, handle_true,
    handle_false,      handle_jump,      handle_jump_if_false, handle_store,
    handle_index,      handle_append,    handle_length,        handle_list,
    handle_const_64,   handle_const_32,  handle_const_8,       handle_string,
    handle_def_local,  handle_get_local, handle_assign,        handle_call,
    handle_return,     handle_print,     handle_pop,           handle_make_list,
    handle_make_tuple,
};

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

        ++vm->call_frame->ip;
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
    free(code->bytes);
    free(code);
    run(vm);
    free_vm(vm);
}
