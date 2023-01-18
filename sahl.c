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

#define MAX_STACK 1024
#define MAX_CALL_DEPTH 1024

// #define PRINT_OPCODES
// #define PRINT_STACK
// #define PRINT_LOCALS
#define UNSAFE

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN ((uint64_t)0x7ffc000000000000)

#define TAG_FALSE 2 // 10.
#define TAG_TRUE 3  // 11.

typedef uint64_t Value;

enum ObjType {
    OBJ_STRING,
    OBJ_LIST,
    OBJ_TUPLE,
};

typedef enum ObjType ObjType;

struct Obj {
    ObjType type;
    union {
        struct {
            char *data;
        } string;
        struct {
            uint64_t capacity;
            uint64_t length;
            Value *items;
            uint8_t owner;
        } list;
        struct {
            uint64_t length;
            Value *items;
        } tuple;
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
    int start_ip = read_u32(code, 0);
    printf("Start IP: %u\n", start_ip);
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
    for (; i < length; i++) {
        printf("%4d: ", i - 4);
        i = print_opcode(code, i);
    }
}

struct VM {
    int ip;
    uint8_t *code;
    int code_length;
    Value *stack;
    int stack_size;
    Value **locals;      // list of local for each function in the call stack
    int *locals_size;    // size of each local list
    int locals_count;    // number of local lists
    int locals_capacity; // capacity of local lists
    int call_depth;
    uint32_t *prev_ips;
    int string_count;
    char **strings;
};

typedef struct VM VM;

void free_value(Value value);

VM *new_vm(uint8_t *code, int code_length, int start_ip) {
    VM *vm = malloc(sizeof(struct VM));
    vm->ip = start_ip;
    vm->string_count = read_u32(code, 0);
    vm->strings = malloc(sizeof(char *) * vm->string_count);
    int offset = 4;
    for (int i = 0; i < vm->string_count; ++i) {
        uint32_t strlength = read_u32(code, offset);
        printf("reading string of %d\n", strlength);
        offset += 4;
        vm->strings[i] = read_string(code, offset, strlength);
        offset += strlength;
    }
    vm->code = code + offset;
    vm->code_length = code_length - offset;
    vm->stack_size = 0;
    vm->stack = malloc(sizeof(Value) * 1024);
    vm->locals_count = 1;
    vm->locals_capacity = 4;
    vm->locals = calloc(sizeof(Value *), MAX_CALL_DEPTH);
    vm->locals_size = calloc(sizeof(int), MAX_CALL_DEPTH);
    vm->prev_ips = calloc(sizeof(uint32_t), MAX_CALL_DEPTH);
    vm->locals_size[0] = 0;
    vm->call_depth = 0;
    return vm;
}

void free_vm(VM *vm) {
    for (int i = 0; i < vm->stack_size; ++i) {
        free_value(vm->stack[i]);
    }
    free(vm->stack);
    for (int i = 0; i < vm->locals_count; ++i) {
        for (int j = 0; j < vm->locals_size[i]; ++j) {
            free_value(vm->locals[i][j]);
        }
        free(vm->locals[i]);
    }
    uint64_t string_lengths = 0;
    for (int i = 0; i < vm->string_count; ++i) {
        string_lengths += strlen(vm->strings[i]);
        free(vm->strings[i]);
    }
    free(vm->strings);
    free(vm->locals);
    free(vm->locals_size);
    free(vm->prev_ips);
    // vm->code points to the start of the code, so we need to subtract the
    // 4 bytes (start ip) + 4 bytes (string count) + string_lengths + 4 * string_count
    uint8_t* code_ptr = vm->code - 8 - string_lengths - 4 * vm->string_count;
    free(code_ptr);
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
        printf("%p ", obj);
        if (obj->type == OBJ_STRING) {
            printf("%s", obj->string.data);
        } else if (obj->type == OBJ_LIST) {
            printf("[");
            for (int i = 0; i < obj->list.length; i++) {
                print_value(obj->list.items[i]);
                printf(", ");
            }
            printf("]");
        } else {
            printf("(");
            for (int i = 0; i < obj->tuple.length; i++) {
                print_value(obj->tuple.items[i]);
                if (i != obj->tuple.length - 1) printf(", ");
            }
            printf(")");
        }
    }
}

void free_value(Value value) {
    if (IS_OBJ(value)) {
        Obj *obj = AS_OBJ(value);
        if (obj->type == OBJ_STRING) {
            free(obj->string.data);
        } else if (obj->type == OBJ_LIST && obj->list.owner) {
            free(obj->list.items);
        } else if (obj->type == OBJ_TUPLE) {
            free(obj->tuple.items);
        }
        free(obj);
    }
}

void run(VM *vm) {
    while (vm->ip < vm->code_length) {
        uint8_t instruction = vm->code[vm->ip];

#ifdef PRINT_OPCODES
        printf("%d ", vm->ip);
        print_opcode(vm->code, vm->ip);
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
        for (int j = 0; j < vm->locals_size[vm->locals_count - 1]; ++j) {
            print_value(vm->locals[vm->locals_count - 1][j]);
            printf(" ");
        }
        printf(" (size: %d)\n", vm->locals_size[vm->locals_count - 1]);
#endif

        switch (instruction) {
        case ADD: {
            Value a = pop(vm);
            Value b = pop(vm);
            push(vm, a + b);
            break;
        }
        case SUB: {
            Value a = pop(vm);
            Value b = pop(vm);
            push(vm, b - a);
            break;
        }
        case MUL: {
            Value a = pop(vm);
            Value b = pop(vm);
            push(vm, b * a);
            break;
        }
        case DIV: {
            Value a = pop(vm);
            Value b = pop(vm);
            push(vm, b / a);
            break;
        }
        case MOD: {
            Value a = pop(vm);
            Value b = pop(vm);
            push(vm, b % a);
            break;
        }
        case NEG: {
            Value a = pop(vm);
            push(vm, -a);
            break;
        }
        case CONST_U32: {
            uint64_t value = read_u32(vm->code, vm->ip + 1);
            push(vm, value);
            vm->ip += 4;
            break;
        }
        case CONST_U64: {
            uint64_t value = read_u64(vm->code, vm->ip + 1);
            push(vm, value);
            vm->ip += 8;
            break;
        }
        case TRUE: {
            push(vm, TRUE_VAL);
            break;
        }
        case FALSE: {
            push(vm, FALSE_VAL);
            break;
        }
        case NOT: {
            Value a = pop(vm);
            push(vm, BOOL_VAL(!AS_BOOL(a)));
            break;
        }
        case AND: {
            Value b = pop(vm);
            Value a = pop(vm);
            push(vm, BOOL_VAL(AS_BOOL(a) && AS_BOOL(b)));
            break;
        }
        case OR: {
            Value b = pop(vm);
            Value a = pop(vm);
            push(vm, BOOL_VAL(AS_BOOL(a) || AS_BOOL(b)));
            break;
        }
        case EQUAL: {
            Value b = pop(vm);
            Value a = pop(vm);
            push(vm, BOOL_VAL(a == b));
            break;
        }
        case NOT_EQUAL: {
            Value b = pop(vm);
            Value a = pop(vm);
            push(vm, BOOL_VAL(a != b));
            break;
        }
        case LESS: {
            Value b = pop(vm);
            Value a = pop(vm);
            push(vm, BOOL_VAL(a < b));
            break;
        }
        case LESS_EQUAL: {
            Value b = pop(vm);
            Value a = pop(vm);
            push(vm, BOOL_VAL(a <= b));
            break;
        }
        case GREATER: {
            Value b = pop(vm);
            Value a = pop(vm);
            push(vm, BOOL_VAL(a > b));
            break;
        }
        case GREATER_EQUAL: {
            Value b = pop(vm);
            Value a = pop(vm);
            push(vm, BOOL_VAL(a >= b));
            break;
        }
        case STRING: {
            uint32_t stridx = read_u32(vm->code, vm->ip + 1);
            char *string = vm->strings[stridx];
            Obj *obj = malloc(sizeof(Obj));
            obj->type = OBJ_STRING;
            obj->string.data = string;
            push(vm, OBJ_VAL(obj));
            vm->ip += 4;
            break;
        }
        case LIST: {
            uint32_t length = read_u32(vm->code, vm->ip + 1);
            Obj *obj = malloc(sizeof(Obj));
            obj->type = OBJ_LIST;
            obj->list.items = malloc(sizeof(Value) * (length ? length : 2) * 2);
            obj->list.length = length;
            for (int i = length - 1; i >= 0; --i) {
                obj->list.items[i] = pop(vm);
            }
            obj->list.capacity = (length ? length : 2) * 2;
            obj->list.owner = 1;
            push(vm, OBJ_VAL(obj));
            vm->ip += 4;
            break;
        }
        case MAKE_TUPLE: {
            uint32_t len = read_u32(vm->code, vm->ip + 1);
            Obj *obj = malloc(sizeof(Obj));
            obj->type = OBJ_TUPLE;
            obj->tuple.items = malloc(sizeof(Value) * len);
            obj->tuple.length = len;
            for (int i = len - 1; i >= 0; --i) {
                obj->tuple.items[i] = pop(vm);
            }
            push(vm, OBJ_VAL(obj));
            vm->ip += 4;
            break;
        }
        case MAKE_LIST: {
            Value def = pop(vm);
            Value len = pop(vm);
            Obj *obj = malloc(sizeof(Obj));
            obj->type = OBJ_LIST;
            obj->list.items = malloc(sizeof(Value) * (len ? len : 2) * 2);
            obj->list.length = len;
            obj->list.capacity = (len ? len : 2) * 2;
            obj->list.owner = 1;
            for (int i = 0; i < len; ++i) {
                obj->list.items[i] = def;
            }
            push(vm, OBJ_VAL(obj));
            break;
        }
        case PRINT: {
            Value value = pop(vm);
            print_value(value);
            putchar('\n');
            break;
        }
        case GET_LOCAL: {
            uint32_t index = read_u32(vm->code, vm->ip + 1);
            push(vm, vm->locals[vm->locals_count - 1][index]);
            vm->ip += 4;
            break;
        }
        case DEF_LOCAL: {
            uint32_t index = read_u32(vm->code, vm->ip + 1);
            if (index >= vm->locals_size[vm->locals_count - 1]) {
                if (vm->locals_size[vm->locals_count - 1] == 0) {
                    vm->locals[vm->locals_count - 1] =
                        malloc(sizeof(Value) * (index ? index * 2 : 2));
                } else {
                    vm->locals[vm->locals_count - 1] =
                        realloc(vm->locals[vm->locals_count - 1],
                                sizeof(Value) * index * 2);
                }
            }
            Value val = pop(vm);
            vm->locals[vm->locals_count - 1][index] = val;
            vm->locals_size[vm->locals_count - 1] = index + 1;
            vm->ip += 4;
            break;
        }
        case ASSIGN: {
            uint32_t index = read_u32(vm->code, vm->ip + 1);
            Value val = pop(vm);
            vm->locals[vm->locals_count - 1][index] = val;
            vm->ip += 4;
            break;
        }
        case CALL: {
            uint32_t funcip = read_u32(vm->code, vm->ip + 1);
            uint32_t argc = read_u32(vm->code, vm->ip + 5);
            Value *args = malloc(sizeof(Value) * argc);
            for (int i = argc - 1; i >= 0; --i) {
                Value val = pop(vm);
                if (IS_OBJ(val)) {
                    Obj *obj = AS_OBJ(val);
                    Obj *new_obj = malloc(sizeof(Obj));
                    if (obj->type == OBJ_LIST) {
                        new_obj->list = obj->list;
                        new_obj->list.owner = 0;
                        new_obj->type = OBJ_LIST;
                    } else {
                        printf("%d", obj->type);
                        int len = strlen(obj->string.data) + 1;
                        new_obj->string.data = malloc(len);
                        memcpy(new_obj->string.data, obj->string.data, len);
                        obj->type = OBJ_STRING;
                    }
                    Value new_val = OBJ_VAL(new_obj);
                    args[i] = new_val;
                } else {
                    args[i] = val;
                }
            }
            vm->locals_count++;
            if (vm->locals_count >= MAX_CALL_DEPTH) {
                error(vm, "Maximum call depth exceeded");
            }
            vm->locals[vm->locals_count - 1] = args;
            vm->locals_size[vm->locals_count - 1] = argc;
            ++vm->call_depth;
            vm->prev_ips[vm->call_depth - 1] = vm->ip + 9;
            vm->ip = funcip - 1;
            break;
        }
        case RETURN: {
            if (vm->call_depth == 0) {
                return;
            }
            vm->ip = vm->prev_ips[vm->call_depth - 1] - 1;
            --vm->call_depth;
            if (vm->stack_size) {
                Value val = pop(vm);
                if (IS_OBJ(val)) {
                    Obj *obj = AS_OBJ(val);
                    Obj *new_obj = malloc(sizeof(Obj));
                    if (obj->type == OBJ_LIST) {
                        new_obj->list.items =
                            malloc(sizeof(Value) * obj->list.capacity);
                        memcpy(new_obj->list.items, obj->list.items,
                               sizeof(Value) * obj->list.length);
                        new_obj->list.length = obj->list.length;
                        new_obj->list.capacity = obj->list.capacity;
                        new_obj->list.owner = 1;
                        new_obj->type = OBJ_LIST;
                        push(vm, OBJ_VAL(new_obj));
                    } else if (obj->type == OBJ_STRING) {
                        int len = strlen(obj->string.data) + 1;
                        new_obj->string.data = malloc(len);
                        memcpy(new_obj->string.data, obj->string.data, len);
                        new_obj->type = OBJ_STRING;
                        push(vm, OBJ_VAL(new_obj));
                    } else {
                        new_obj->type = OBJ_TUPLE;
                        new_obj->tuple.length = obj->tuple.length;
                        new_obj->tuple.items =
                            malloc(sizeof(Value) * obj->tuple.length);
                        memcpy(new_obj->tuple.items, obj->tuple.items,
                               sizeof(Value) * obj->tuple.length);
                        push(vm, OBJ_VAL(new_obj));
                    }
                } else {
                    push(vm, val);
                }
            }
            for (int i = 0; i < vm->locals_size[vm->locals_count - 1]; ++i) {
                free_value(vm->locals[vm->locals_count - 1][i]);
            }
            free(vm->locals[vm->locals_count - 1]);
            vm->locals_size[vm->locals_count - 1] = 0;
            --vm->locals_count;
            break;
        }
        case JUMP: {
            uint32_t ip = read_u32(vm->code, vm->ip + 1);
            vm->ip = ip - 1;
            break;
        }
        case JUMP_IF_FALSE: {
            uint32_t ip = read_u32(vm->code, vm->ip + 1);
            Value value = pop(vm);
            if (!AS_BOOL(value)) {
                vm->ip = ip - 1;
            } else {
                vm->ip += 4;
            }
            break;
        }
        case STORE: {
            Value index = pop(vm);
            Value arr = pop(vm);
            Value value = pop(vm);
            Obj *obj = AS_OBJ(arr);
            obj->list.items[index] = value;
            break;
        }
        case APPEND: {
            Value value = pop(vm);
            Value list = pop(vm);
            Obj *obj = AS_OBJ(list);
            if (obj->list.length >= obj->list.capacity) {
                obj->list.capacity *= 2;
                obj->list.items = realloc(obj->list.items,
                                          sizeof(Value) * obj->list.capacity);
            }
            obj->list.items[obj->list.length] = value;
            obj->list.length++;
            break;
        }
        case INDEX: {
            Value index = pop(vm);
            Value value = pop(vm);
            Obj *obj = AS_OBJ(value);
            push(vm, obj->list.items[index]);
            break;
        }
        case LENGTH: {
            Value value = pop(vm);
            Obj *obj = AS_OBJ(value);
            push(vm, obj->list.length);
            break;
        }
        case POP: {
            pop(vm);
            break;
        }
        default: {
            char msg[100];
            sprintf(msg, "Unknown opcode %d", vm->code[vm->ip]);
            error(vm, msg);
        }
        }
        ++vm->ip;
    }
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
    VM *vm =
        new_vm(code->bytes + 4, code->length - 4, read_u32(code->bytes, 0));
    free(code);
    run(vm);
    free_vm(vm);
}
