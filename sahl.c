#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

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

struct Value {
    uint8_t type;
    union {
        uint8_t u8;
        unsigned int u32;
        unsigned long long u64;
        char *string;
        struct ValueList *list;
    } val;
};

struct ValueList {
    struct Value *values;
    unsigned int length;
};

struct Code {
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

void dissassemble(uint8_t *code, int length) {
    int start_ip = read_u32(code, 0);
    int i = 4;
    printf("start ip: %d\n", start_ip);
    for (i = 4; i < length; i++) {
        printf("%4d: ", i - 4);
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
            printf("Jump %lu\n", read_u64(code, i + 1));
            i += 8;
            break;
        case JUMP_IF_FALSE:
            // u64 code
            printf("JumpIfFalse %lu\n", read_u64(code, i + 1));
            i += 8;
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
    free(code->bytes);
    free(code);
}
