#include "debug.h"
#include "opcodes.h"
#include "read.h"
#include <stdio.h>
#include <stdlib.h>

int print_opcode(uint8_t *code, int i) {
    switch (code[i]) {
    case IADD:
        printf("IAdd\n");
        break;
    case ISUB:
        printf("ISub\n");
        break;
    case IMUL:
        printf("IMul\n");
        break;
    case IDIV:
        printf("IDiv\n");
        break;
    case IMOD:
        printf("IMod\n");
        break;
    case INEG:
        printf("INeg\n");
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
    case ILESS:
        printf("Less\n");
        break;
    case ILESS_EQUAL:
        printf("LessEqual\n");
        break;
    case IGREATER:
        printf("IGreater\n");
        break;
    case IGREATER_EQUAL:
        printf("IGreaterEqual\n");
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
    case LIST_INDEX:
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
        printf("MakeList\n");
        break;
    case STRING: {
        // u32 length
        uint32_t stridx = read_u32(code, i + 1);
        i += 5;
        printf("string at index %d\n", stridx);
        break;
    }
    case LIST_APPEND:
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
    case LIST_LENGTH:
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
    case NATIVE_CALL:
        // u32 function index, u32 arg count
        printf("NativeCall \t fn: %u \t arg count: %u \n",
               read_u32(code, i + 1), read_u32(code, i + 5));
        i += 8;
        break;
    case CONST_DOUBLE:
        // u64 code
        printf("ConstDouble %f\n", read_double(code, i + 1));
        i += 8;
        break;
    case MAKE_CHAN:
        printf("MakeChan\n");
        break;
    case SPAWN:
        printf("Spawn\n");
        break;
    case CHAN_WRITE:
        printf("ChanWrite\n");
        break;
    case CHAN_READ:
        printf("ChanRead\n");
        break;
    case MAKE_MAP:
        printf("MakeMap\n");
        break;
    case FADD:
        printf("FAdd\n");
        break;
    case FSUB:
        printf("FSub\n");
        break;
    case FMUL:
        printf("FMul\n");
        break;
    case FDIV:
        printf("FDiv\n");
        break;
    case FNEG:
        printf("FNeg\n");
        break;
    case FLESS:
        printf("FLess\n");
        break;
    case FLESS_EQUAL:
        printf("FLessEqual\n");
        break;
    case FGREATER:
        printf("FGreater\n");
        break;
    case FGREATER_EQUAL:
        printf("FGreaterEqual\n");
        break;
    case SCONCAT:
        printf("SConcat\n");
        break;
    case I2F:
        printf("I2F\n");
        break;
    case I2S:
        printf("I2S\n");
        break;
    case F2S:
        printf("F2S\n");
        break;
    case FMOD:
        printf("FMod\n");
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
        int argc = read_u32(code, i);
        i += 4;
        printf("func %d - length %d - argc %d\n", j, func_length, argc);
        int start_ = i;
        int end = i + func_length;
        while (i < end) {
            printf("%4d ", i - start_);
            i = print_opcode(code, i);
            ++i;
        }
    }
}