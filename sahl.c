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

#define MAX_STACK 1024
#define MAX_CALL_DEPTH 256

// #define PRINT_OPCODES
// #define PRINT_STACK
// #define PRINT_LOCALS

enum ValueType {
    VALUE_TYPE_U64,
    VALUE_TYPE_U32,
    VALUE_TYPE_U8,
    VALUE_TYPE_STRING,
    VALUE_TYPE_LIST,
    VALUE_TYPE_BOOL,
};

typedef enum ValueType ValueType;

struct Value {
    ValueType type;
    union {
        uint8_t u8;
        unsigned int u32;
        unsigned long long u64;
        char *string;
        struct ValueList *list;
    } val;
};

typedef struct Value Value;

struct ValueList {
    struct Value *values;
    unsigned int length;
};

typedef struct ValueList ValueList;

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
    uint32_t res = 0;
    res |= (uint32_t)code[idx] << 24;
    res |= (uint32_t)code[idx + 1] << 16;
    res |= (uint32_t)code[idx + 2] << 8;
    res |= (uint32_t)code[idx + 3];
    return res;
}

uint64_t read_u64(uint8_t *code, int idx) {
    uint64_t res = 0;
    res |= (uint64_t)code[idx] << 56;
    res |= (uint64_t)code[idx + 1] << 48;
    res |= (uint64_t)code[idx + 2] << 40;
    res |= (uint64_t)code[idx + 3] << 32;
    res |= (uint64_t)code[idx + 4] << 24;
    res |= (uint64_t)code[idx + 5] << 16;
    res |= (uint64_t)code[idx + 6] << 8;
    res |= (uint64_t)code[idx + 7];
    return res;
}

char *read_string(uint8_t *code, int idx, int len) {
    char *str = malloc(len + 1);
    for (int i = 0; i < len; i++) {
        str[i] = code[idx + i];
    }
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
    case STRING: {
        ++i; // ignore CONST_U32
        // u32 length
        uint32_t strlength = read_u32(code, i + 1);
        printf("String len: %u string = ", strlength);
        i += 4;
        char *string = read_string(code, i + 1, strlength);
        printf("%s\n", string);
        i += strlength;
        free(string);
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
    for (int i = 4; i < length; i++) {
        printf("%4d: ", i - 4);
        i = print_opcode(code, i);
    }
}

struct VM {
    int ip;
    uint8_t *code;
    int code_length;
    struct Value **stack;
    int stack_size;
    struct Value **locals; // list of local for each function in the call stack
    int *locals_size;      // size of each local list
    int locals_count;      // number of local lists
    int locals_capacity;   // capacity of local lists
    int call_depth;
    uint32_t *prev_ips;
};

struct VM *vm;

void free_value(Value *value);

void new_vm(uint8_t *code, int code_length, int start_ip) {
    vm = malloc(sizeof(struct VM));
    vm->ip = start_ip;
    vm->code = code;
    vm->code_length = code_length;
    vm->stack_size = 0;
    vm->stack = malloc(sizeof(struct Value) * 1024);
    vm->locals_count = 1;
    vm->locals_capacity = 4;
    vm->locals = calloc(sizeof(struct Value *), MAX_CALL_DEPTH);
    vm->locals[0] = malloc(sizeof(struct Value) * 4);
    vm->locals_size = calloc(sizeof(int), MAX_CALL_DEPTH);
    vm->locals_size[0] = 0;
    vm->call_depth = 0;
}

void free_vm() {
    free(vm->locals);
    free(vm->locals_size);
    free(vm->stack);
    free(vm);
}

void error(char *msg) {
    printf("Error: %s", msg);
    exit(1);
}

struct Value *pop() {
    if (vm->stack_size == 0) {
        error("Stack underflow");
    }
    return vm->stack[--vm->stack_size];
}

struct Value *peek() {
    if (vm->stack_size == 0) {
        error("Stack underflow");
    }
    return vm->stack[vm->stack_size - 1];
}

void push(struct Value *value) {
    if (vm->stack_size == MAX_STACK) {
        error("Stack overflow");
    }
    vm->stack[vm->stack_size++] = value;
}

struct Value *get_local(int index) {
    if (index >= vm->locals_count) {
        error("Local variable not found");
    }
    return vm->locals[index] + (vm->locals_size[index] - 1);
}

void set_local(int index, struct Value value) {
    if (index >= vm->locals_count) {
        error("Local variable not found");
    }
    vm->locals[index][vm->locals_size[index] - 1] = value;
}

void push_local(int index, struct Value value) {
    if (index >= vm->locals_count) {
        error("Local variable not found");
    }
    if (vm->locals_size[index] == MAX_STACK) {
        error("Local variable stack overflow");
    }
    vm->locals[index][vm->locals_size[index]++] = value;
}

void pop_local(int index) {
    if (index >= vm->locals_count) {
        error("Local variable not found");
    }
    if (vm->locals_size[index] == 0) {
        error("Local variable stack underflow");
    }
    --vm->locals_size[index];
}

char *value_type_to_string(enum ValueType type) {
    switch (type) {
    case VALUE_TYPE_U64:
        return "INT";
    case VALUE_TYPE_STRING:
        return "STRING";
    case VALUE_TYPE_LIST:
        return "LIST";
    default: {
        char *res = calloc(sizeof(char), 7 + 19);
        sprintf(res, "UNKNOWN %d", type);
        return res;
    }
    }
}

struct Value *new_u32(uint32_t value) {
    struct Value *v = malloc(sizeof(struct Value));
    v->type = VALUE_TYPE_U32;
    v->val.u32 = value;
    return v;
}

struct Value *new_u64(uint64_t value) {
    struct Value *v = malloc(sizeof(struct Value));
    v->type = VALUE_TYPE_U64;
    v->val.u64 = value;
    return v;
}

struct Value *new_u8(uint8_t value) {
    struct Value *v = malloc(sizeof(struct Value));
    v->type = VALUE_TYPE_U8;
    v->val.u8 = value;
    return v;
}

void print_value(Value *value) {
    switch (value->type) {
    case VALUE_TYPE_U32:
        printf("%u", value->val.u32);
        break;
    case VALUE_TYPE_U64:
        printf("%llu", value->val.u64);
        break;
    case VALUE_TYPE_STRING:
        printf("%s", value->val.string);
        break;
    case VALUE_TYPE_LIST:
        printf("[");
        for (int i = 0; i < value->val.list->length; ++i) {
            print_value(value->val.list->values + i);
            if (i != value->val.list->length - 1) {
                printf(", ");
            }
        }
        printf("]");
        break;
    case VALUE_TYPE_U8:
        printf("%s", value->val.u8 ? "true" : "false");
        break;
    default: {
        char msg[100];
        sprintf(msg, "Unknown value type %u", value->type);
        error(msg);
    }
    }
}

void free_value(Value *value) {
    switch (value->type) {
        {
        case VALUE_TYPE_STRING:
            free(value->val.string);
            break;
        case VALUE_TYPE_LIST:
            free(value->val.list->values);
            free(value->val.list);
            break;
        }
    }
    free(value);
}

void run() {
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
            print_value(vm->locals[vm->locals_count - 1] + j);
            printf(" ");
        }
        printf(" (size: %d)\n", vm->locals_size[vm->locals_count - 1]);
#endif

        switch (instruction) {
        case ADD: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u64(a->val.u64 + b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for ADD");
            }
            break;
        }
        case SUB: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u64(a->val.u64 - b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for SUB");
            }
            break;
        }
        case MUL: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u64(a->val.u64 * b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for SUB");
            }
            break;
        }
        case DIV: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u64(a->val.u64 / b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for SUB");
            }
            break;
        }
        case MOD: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u64(a->val.u64 % b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for SUB");
            }
            break;
        }
        case NEG: {
            struct Value *a = pop();
            push(new_u64(-a->val.u64));
            free(a);
            break;
        }
        case CONST_U32: {
            uint32_t value = read_u32(vm->code, vm->ip + 1);
            push(new_u32(value));
            vm->ip += 4;
            break;
        }
        case CONST_U64: {
            uint64_t value = read_u64(vm->code, vm->ip + 1);
            push(new_u64(value));
            vm->ip += 8;
            break;
        }
        case TRUE: {
            push(new_u8(1));
            break;
        }
        case FALSE: {
            push(new_u8(0));
            break;
        }
        case NOT: {
            struct Value *a = pop();
            push(new_u8(!a->val.u8));
            free(a);
            break;
        }
        case AND: {
            struct Value *b = pop();
            struct Value *a = pop();
            push(new_u8(a->val.u8 && b->val.u8));
            free(a);
            free(b);
            break;
        }
        case OR: {
            struct Value *b = pop();
            struct Value *a = pop();
            push(new_u8(a->val.u8 || b->val.u8));
            free(a);
            free(b);
            break;
        }
        case EQUAL: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u8(a->val.u64 == b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for EQUAL");
            }
            break;
        }
        case NOT_EQUAL: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u8(a->val.u64 != b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for NOT_EQUAL");
            }
            break;
        }
        case LESS: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u8(a->val.u64 < b->val.u64));
                free(a);
                free(b);
            } else {
                char *a_type = value_type_to_string(a->type);
                char *b_type = value_type_to_string(b->type);
                printf("Invalid types for LESS: %s and %s", a_type, b_type);
                error("");
            }
            break;
        }
        case LESS_EQUAL: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u8(a->val.u64 <= b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for LESS_EQUAL");
            }
            break;
        }
        case GREATER: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u8(a->val.u64 > b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for GREATER");
            }
            break;
        }
        case GREATER_EQUAL: {
            struct Value *b = pop();
            struct Value *a = pop();
            if (a->type == VALUE_TYPE_U64 && b->type == VALUE_TYPE_U64) {
                push(new_u8(a->val.u64 >= b->val.u64));
                free(a);
                free(b);
            } else {
                error("Invalid types for GREATER_EQUAL");
            }
            break;
        }
        case STRING: {
            uint32_t length = read_u32(vm->code, vm->ip + 2);
            char *string = read_string(vm->code, vm->ip + 6, length);
            struct Value *value = malloc(sizeof(struct Value));
            value->type = VALUE_TYPE_STRING;
            value->val.string = string;
            push(value);
            vm->ip += 5 + length;
            break;
        }
        case LIST: {
            uint32_t length = read_u32(vm->code, vm->ip + 1);
            struct Value *value = malloc(sizeof(struct Value));
            value->type = VALUE_TYPE_LIST;
            value->val.list = malloc(sizeof(ValueList));
            value->val.list->length = length;
            value->val.list->values = malloc(sizeof(struct Value) * length);
            for (int i = length - 1; i >= 0; --i) {
                Value *item = pop();
                value->val.list->values[i] = *item;
                free(item);
            }
            push(value);
            vm->ip += 4;
            break;
        }
        case PRINT: {
            struct Value *value = pop();
            print_value(value);
            putchar('\n');
            free(value);
            break;
        }
        case GET_LOCAL: {
            uint32_t index = read_u32(vm->code, vm->ip + 1);
            Value *val = malloc(sizeof(Value));
            memcpy(val, vm->locals[vm->locals_count - 1] + index,
                   sizeof(Value));
            push(val);
            vm->ip += 4;
            break;
        }
        case DEF_LOCAL: {
            uint32_t index = read_u32(vm->code, vm->ip + 1);
            if (index >= vm->locals_size[vm->locals_count - 1]) {
                if (vm->locals_size[vm->locals_count - 1] == 0) {
                    vm->locals[vm->locals_count - 1] =
                        malloc(sizeof(struct Value) * index * 2);
                } else {
                    vm->locals[vm->locals_count - 1] =
                        realloc(vm->locals[vm->locals_count - 1],
                                sizeof(struct Value) * index * 2);
                }
            }
            Value *val = pop();
            vm->locals[vm->locals_count - 1][index] = *val;
            free(val);
            vm->locals_size[vm->locals_count - 1] = index + 1;
            vm->ip += 4;
            break;
        }
        case ASSIGN: {
            uint32_t index = read_u32(vm->code, vm->ip + 1);
            Value *val = pop();
            vm->locals[vm->locals_count - 1][index] = *val;
            free(val);
            vm->ip += 4;
            break;
        }
        case CALL: {
            uint32_t funcip = read_u32(vm->code, vm->ip + 1);
            uint32_t argc = read_u32(vm->code, vm->ip + 5);
            struct Value *args = malloc(sizeof(struct Value) * argc);
            for (int i = argc - 1; i >= 0; --i) {
                Value *val = pop();
                args[i] = *val;
                free(val);
            }
            vm->locals_count++;
            if (vm->locals_count >= MAX_CALL_DEPTH) {
                error("Maximum call depth exceeded");
            }
            vm->locals[vm->locals_count - 1] = args;
            vm->locals_size[vm->locals_count] = argc + 1;
            ++vm->locals_count;
            ++vm->call_depth;
            vm->prev_ips =
                realloc(vm->prev_ips, sizeof(uint32_t) * (vm->call_depth));
            vm->prev_ips[vm->call_depth - 1] = vm->ip;
            vm->ip = funcip - 1;
            break;
        }
        case RETURN: {
            if (vm->call_depth == 0) {
                return;
            }
            vm->ip = vm->prev_ips[vm->call_depth - 1];
            --vm->call_depth;
            vm->prev_ips =
                realloc(vm->prev_ips, sizeof(uint32_t) * vm->call_depth);
            // printf("locals length %d\n", vm->locals_count);
            // printf("locals_count: %d\n", vm->locals_size[vm->locals_count]);
            for (int i = 0; i < vm->locals_size[vm->locals_count - 1]; ++i) {
                print_value(vm->locals[vm->locals_count - 1] + i);
                putchar('\n');
                free(vm->locals[vm->locals_count - 1] + i);
            }
            free(vm->locals + vm->locals_count);
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
            struct Value *value = pop();
            if (value->type == VALUE_TYPE_U8 && value->val.u8 == 0) {
                vm->ip = ip - 1;
            } else {
                vm->ip += 4;
            }
            free(value);
            break;
        }
        case STORE: {
            uint32_t index = read_u32(vm->code, vm->ip + 1);
            struct Value *arr = pop();
            if (arr->type == VALUE_TYPE_LIST) {
                struct Value *value = pop();
                arr->val.list->values[index] = *value;
                free(value);
            } else {
                error("Invalid type for STORE");
            }
            vm->ip += 4;
            break;
        }
        case APPEND: {
            struct Value *value = pop();
            struct Value *list = pop();
            if (list->type == VALUE_TYPE_LIST) {
                list->val.list->length += 1;
                list->val.list->values =
                    realloc(list->val.list->values,
                            sizeof(struct Value) * list->val.list->length);
                list->val.list->values[list->val.list->length - 1] = *value;
                push(list);
                free(value);
            } else {
                // printf("%d ", list->type);
                error("Invalid type for APPEND");
            }
            break;
        }
        case INDEX: {
            struct Value *index = pop();
            struct Value *value = pop(); // list shouldnt be freed
            if (value->type == VALUE_TYPE_LIST) {
                Value *val = malloc(sizeof(struct Value));
                *val = value->val.list->values[index->val.u64];
                push(val);
                free(index);
            } else {
                char msg[100];
                sprintf(msg, "Invalid type for INDEX: %d", value->type);
                error(msg);
            }
            break;
        }
        case LENGTH: {
            struct Value *value = pop();
            if (value->type == VALUE_TYPE_LIST) {
                Value *val = malloc(sizeof(struct Value));
                val->type = VALUE_TYPE_U64;
                val->val.u64 = value->val.list->length;
                push(val);
            } else {
                error("Invalid type for LENGTH");
            }
            break;
        }
        case POP: {
            free(pop());
            break;
        }
        default: {
            char msg[100];
            sprintf(msg, "Unknown opcode %d", vm->code[vm->ip]);
            error(msg);
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
    new_vm(code->bytes + 4, code->length - 4, read_u32(code->bytes, 0));
    run();
    free(code->bytes);
    free(code);
    free_vm();
}
