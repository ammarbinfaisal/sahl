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

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_FALSE 2 // 10.
#define TAG_TRUE  3 // 11.

typedef uint64_t Value;

enum ObjType {
    OBJ_STRING,
    OBJ_LIST,
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
        } list;
    };
};

typedef struct Obj Obj;

#define IS_BOOL(value)      (((value) | 1) == TRUE_VAL)
#define IS_NUMBER(value)    (((value) & QNAN) != QNAN)
#define IS_OBJ(value) \
    (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value)      ((value) == TRUE_VAL)
#define AS_NUMBER(value)    valueToNum(value)
#define AS_OBJ(value) \
    ((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))


static inline uint64_t valueToNum(Value value) {
  uint64_t num;
  memcpy(&num, &value, sizeof(Value));
  return num;
}

static inline Value numToValue(uint64_t num) {
  Value value;
  memcpy(&value, &num, sizeof(uint64_t));
  return value;
}

#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NUMBER_VAL(num) numToValue(num)
#define OBJ_VAL(obj) \
    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

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
    Value *stack;
    int stack_size;
    Value **locals; // list of local for each function in the call stack
    int *locals_size;      // size of each local list
    int locals_count;      // number of local lists
    int locals_capacity;   // capacity of local lists
    int call_depth;
    uint32_t *prev_ips;
};

struct VM *vm;

void free_value(Value value);

void new_vm(uint8_t *code, int code_length, int start_ip) {
    vm = malloc(sizeof(struct VM));
    vm->ip = start_ip;
    vm->code = code;
    vm->code_length = code_length;
    vm->stack_size = 0;
    vm->stack = malloc(sizeof(Value) * 1024);
    vm->locals_count = 1;
    vm->locals_capacity = 4;
    vm->locals = calloc(sizeof(Value *), MAX_CALL_DEPTH);
    vm->locals[0] = malloc(sizeof(Value) * 4);
    vm->locals_size = calloc(sizeof(int), MAX_CALL_DEPTH);
    vm->locals_size[0] = 0;
    vm->call_depth = 0;
}

void free_vm() {
    for (int i = 0; i < vm->stack_size; ++i) {
        free_value(vm->stack[i]);
    }
    free(vm->stack);
    for (int i = 0; i < vm->locals_count; ++i) {
        for (int j = 0; j < vm->locals_size[i]; ++j) {
            free_value(vm->locals[i][j]);
        }
    }
    free(vm->locals);
    free(vm->locals_size);
    free(vm);
}

void error(char *msg) {
    printf("Error: %s", msg);
    exit(1);
}

Value pop() {
    if (vm->stack_size == 0) {
        error("Stack underflow");
    }
    return vm->stack[--vm->stack_size];
}

Value peek() {
    if (vm->stack_size == 0) {
        error("Stack underflow");
    }
    return vm->stack[vm->stack_size - 1];
}

void push(Value value) {
    if (vm->stack_size == MAX_STACK) {
        error("Stack overflow");
    }
    vm->stack[vm->stack_size++] = value;
}

void print_value(Value value) {
    if (IS_BOOL(value)) {
        printf("%s", AS_BOOL(value) ? "true" : "false");
    } else if (IS_NUMBER(value)) {
        printf("%ld", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        Obj* obj = AS_OBJ(value);
        if (obj->type == OBJ_STRING) {
            printf("%s", obj->string.data);
        } else if (obj->type == OBJ_LIST) {
            printf("[");
            for (int i = 0; i < obj->list.length; i++) {
                print_value(obj->list.items[i]);
                printf(", ");
            }
            printf("]");
        }
    }
}

void free_value(Value value) {
    if (IS_OBJ(value)) {
        Obj* obj = AS_OBJ(value);
        if (obj->type == OBJ_STRING) {
            free(obj->string.data);
        } else if (obj->type == OBJ_LIST) {
            for (int i = 0; i < obj->list.length; i++) {
                free_value(obj->list.items[i]);
            }
            free(obj->list.items);
        }
        free(obj);
    }
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
            print_value(vm->locals[vm->locals_count - 1][j]);
            printf(" ");
        }
        printf(" (size: %d)\n", vm->locals_size[vm->locals_count - 1]);
#endif

        switch (instruction) {
        case ADD: {
            Value a = pop();
            Value b = pop();
            push(NUMBER_VAL(AS_NUMBER(a) + AS_NUMBER(b)));
            break;
        }
        case SUB: {
            Value a = pop();
            Value b = pop();
            push(NUMBER_VAL(AS_NUMBER(b) - AS_NUMBER(a)));
            break;
        }
        case MUL: {
            Value a = pop();
            Value b = pop();
            push(NUMBER_VAL(AS_NUMBER(b) * AS_NUMBER(a)));
            break;
        }
        case DIV: {
            Value a = pop();
            Value b = pop();
            push(NUMBER_VAL(AS_NUMBER(b) / AS_NUMBER(a)));
            break;
        }
        case MOD: {
            Value a = pop();
            Value b = pop();
            push(NUMBER_VAL(AS_NUMBER(b) % AS_NUMBER(a)));
            break;
        }
        case NEG: {
            Value a = pop();
            push(NUMBER_VAL(-AS_NUMBER(a)));
            break;
        }
        case CONST_U32: {
            uint64_t value = read_u32(vm->code, vm->ip + 1);
            push(value);
            vm->ip += 4;
            break;
        }
        case CONST_U64: {
            uint64_t value = read_u64(vm->code, vm->ip + 1);
            push(value);
            vm->ip += 8;
            break;
        }
        case TRUE: {
            push(TRUE_VAL);
            break;
        }
        case FALSE: {
            push(FALSE_VAL);
            break;
        }
        case NOT: {
            Value a = pop();
            push(BOOL_VAL(!AS_BOOL(a)));
            break;
        }
        case AND: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(AS_BOOL(a) && AS_BOOL(b)));
            break;
        }
        case OR: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(AS_BOOL(a) || AS_BOOL(b)));
            break;
        }
        case EQUAL: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(AS_NUMBER(a) == AS_NUMBER(b)));
            break;
        }
        case NOT_EQUAL: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(AS_NUMBER(a) != AS_NUMBER(b)));
            break;
        }
        case LESS: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(AS_NUMBER(a) < AS_NUMBER(b)));
            break;
        }
        case LESS_EQUAL: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(AS_NUMBER(a) <= AS_NUMBER(b)));
            break;
        }
        case GREATER: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(AS_NUMBER(a) > AS_NUMBER(b)));
            break;
        }
        case GREATER_EQUAL: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(AS_NUMBER(a) >= AS_NUMBER(b)));
            break;
        }
        case STRING: {
            uint32_t length = read_u32(vm->code, vm->ip + 2);
            char *string = read_string(vm->code, vm->ip + 6, length);
            Obj *obj = malloc(sizeof(Obj));
            obj->type = OBJ_STRING;
            obj->string.data = string;
            push(OBJ_VAL(obj));
            vm->ip += 5 + length;
            break;
        }
        case LIST: {
            uint32_t length = read_u32(vm->code, vm->ip + 1);
            Obj *obj = malloc(sizeof(Obj));
            obj->type = OBJ_LIST;
            obj->list.items = malloc(sizeof(Value) * (length ? length : 2) * 2);
            obj->list.length = length;
            for (int i = length - 1; i >= 0; --i) {
                obj->list.items[i] = pop();
            }
            obj->list.capacity = (length ? length : 2) * 2;
            push(OBJ_VAL(obj));
            vm->ip += 4;
            break;
        }
        case PRINT: {
            Value value = pop();
            print_value(value);
            putchar('\n');
            break;
        }
        case GET_LOCAL: {
            uint32_t index = read_u32(vm->code, vm->ip + 1);
            push(vm->locals[vm->locals_count - 1][index]);
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
            Value val = pop();
            vm->locals[vm->locals_count - 1][index] = val;
            vm->locals_size[vm->locals_count - 1] = index + 1;
            vm->ip += 4;
            break;
        }
        case ASSIGN: {
            uint32_t index = read_u32(vm->code, vm->ip + 1);
            Value val = pop();
            vm->locals[vm->locals_count - 1][index] = val;
            vm->ip += 4;
            break;
        }
        case CALL: {
            uint32_t funcip = read_u32(vm->code, vm->ip + 1);
            uint32_t argc = read_u32(vm->code, vm->ip + 5);
            Value *args = malloc(sizeof(Value) * argc);
            for (int i = argc - 1; i >= 0; --i) {
                Value val = pop();
                args[i] = val;
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
                print_value(vm->locals[vm->locals_count - 1][i]);
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
            Value value = pop();
            if (!AS_BOOL(value)) {
                vm->ip = ip - 1;
            } else {
                vm->ip += 4;
            }
            break;
        }
        case STORE: {
            Value index = pop();
            Value arr = pop();
            Value value = pop();
            Obj* obj = AS_OBJ(arr);
            if (obj->type == OBJ_LIST) {
                obj->list.items[AS_NUMBER(index)] = value;
            } else {
                error("Cannot store to non-list object");
            }
            break;
        }
        case APPEND: {
            Value value = pop();
            Value list = pop();
            Obj* obj = AS_OBJ(list);
            if (obj->type == OBJ_LIST) {
                if (obj->list.length >= obj->list.capacity) {
                    puts("reallocating");
                    obj->list.capacity *= 2;
                    obj->list.items = realloc(obj->list.items, sizeof(Value) * obj->list.capacity);
                }
                obj->list.items[obj->list.length] = value;
                obj->list.length++;
            } else {
                error("Cannot append to non-list object");
            }
            break;
        }
        case INDEX: {
            Value index = pop();
            Value value = pop();
            Obj* obj = AS_OBJ(value);
            if (obj->type == OBJ_LIST) {
                push(obj->list.items[AS_NUMBER(index)]);
            } else {
                error("Cannot index non-list object");
            }
            break;
        }
        case LENGTH: {
            Value value = pop();
            Obj* obj = AS_OBJ(value);
            if (obj->type == OBJ_LIST) {
                push(NUMBER_VAL(obj->list.length));
            } else {
                error("Cannot get length of non-list object");
            }
            break;
        }
        case POP: {
            pop();
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
