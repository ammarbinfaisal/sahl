#include "debug.h"
#include "opcodes.h"
#include "read.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define print_bits(x)                                                          \
    do {                                                                       \
        unsigned long long a__ = (x);                                          \
        size_t bits__ = sizeof(x) * 8;                                         \
        printf(#x ": ");                                                       \
        while (bits__--)                                                       \
            putchar(a__ & (1ULL << bits__) ? '1' : '0');                       \
        putchar('\n');                                                         \
    } while (0)

int print_opcode(uint8_t *code, int i) {
    switch (code[i]) {
    case OP_IADD:
        printf("iadd %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_ISUB:
        printf("isub %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_IMUL:
        printf("imul %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_IDIV:
        printf("idiv %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_IREM:
        printf("irem %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_INE:
        printf("ine %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_IEQ:
        printf("ieq %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_ILT:
        printf("ilt %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_ILE:
        printf("ile %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_IGT:
        printf("igt %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_IGE:
        printf("ige %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FADD:
        printf("fadd %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FSUB:
        printf("fsub %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FMUL:
        printf("fmul %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FDIV:
        printf("fdiv %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FREM:
        printf("frem %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FNE:
        printf("fne %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FEQ:
        printf("feq %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FLT:
        printf("flt %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FLE:
        printf("fle %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FGT:
        printf("fgt %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FGE:
        printf("fge %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_BAND:
        printf("band %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_BOR:
        printf("bor %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_BXOR:
        printf("bxor %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_BNOT:
        printf("bnot %d - %d\n", code[i + 1], code[i + 2]);
        return i + 3;
    case OP_LAND:
        printf("land %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_LOR:
        printf("lor %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_LNOT:
        printf("lnot %d - %d\n", code[i + 1], code[i + 2]);
        return i + 3;
    case OP_BSHL:
        printf("bshl %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_BSHR:
        printf("bshr %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
    case OP_FNEG:
        printf("fneg %d, - %d\n", code[i + 1], code[i + 2]);
        return i + 3;
    case OP_INEG:
        printf("ineg %d, - %d\n", code[i + 1], code[i + 2]);
        return i + 3;
    case OP_MAKE:
        printf("make reg: %d, size: %d, type: %d\n", code[i + 1], code[i + 2],
               code[i + 3]);
        return i + 4;
    case OP_LISTSET:
        printf("listset %d, %d, %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
        break;
    case OP_LISTGET:
        printf("listget %d, %d - %d\n", code[i + 1], code[i + 2], code[i + 3]);
        return i + 4;
        break;
    case OP_TUPLESET: {
        uint8_t tup_reg = code[i + 1];
        uint8_t idx_reg = code[i + 2];
        uint8_t res_reg = code[i + 3];
        printf("tupleset %d %d - %d\n", tup_reg, idx_reg, res_reg);
        return i + 4;
    }
    case OP_TUPLEGET: {
        uint8_t tup_reg = code[i + 1];
        uint8_t idx_reg = code[i + 2];
        uint8_t res_reg = code[i + 3];
        printf("tupleget %d %d - %d\n", tup_reg, idx_reg, res_reg);
        return i + 4;
    }
    case OP_TUPLE: {
        uint64_t len = read_u64(code, i + 1);
        uint8_t res = code[i + 5];
        printf("tuple %ld - %d\n", len, res);
        return i + 6;
    }
    case OP_STRGET: {
        uint8_t str_reg = code[i + 1];
        uint8_t idx_reg = code[i + 2];
        uint8_t res_reg = code[i + 3];
        printf("strget %d %d - %d\n", str_reg, idx_reg, res_reg);
        return i + 4;
    }
    case OP_MAPGET: {
        uint8_t map_reg = code[i + 1];
        uint8_t key_reg = code[i + 2];
        uint8_t res_reg = code[i + 3];
        printf("mapget %d %d - %d\n", map_reg, key_reg, res_reg);
        return i + 4;
    }
    case OP_MAPSET: {
        uint8_t map_reg = code[i + 1];
        uint8_t key_reg = code[i + 2];
        uint8_t val_reg = code[i + 3];
        printf("mapset %d %d %d\n", map_reg, key_reg, val_reg);
        return i + 4;
    }
    case OP_CHANSEND: {
        uint16_t chan_var = read_u64(code, i + 1);
        uint8_t val_reg = code[i + 3];
        printf("chansend %d - %d\n", chan_var, val_reg);
        return i + 4;
    }
    case OP_CHANRECV: {
        uint16_t chan_var = read_u64(code, i + 1);
        uint8_t res_reg = code[i + 3];
        printf("chanrecv %d - %d\n", chan_var, res_reg);
        return i + 4;
    }
    case OP_JMP: {
        uint64_t addr = read_u64(code, i + 1);
        printf("jmp %ld\n", addr);
        return i + 9;
    }
    case OP_JMPNOT: {
        uint8_t r1 = code[i + 1];
        uint64_t addr = read_u64(code, i + 2);
        printf("jmpnot %d %ld\n", r1, addr);
        return i + 10;
    }
    case OP_CALL: {
        uint64_t ix = read_u64(code, i + 1);
        uint64_t argc = read_u64(code, i + 9);
        printf("call %ld - argc %ld\n", ix, argc);
        return i + 17 + argc;
    }
    case OP_NCALL: {
        uint8_t ix = code[i + 1];
        uint64_t args_count = read_u64(code, i + 2);
        printf("ncall %d - args_count %ld -", ix, args_count);
        for (int j = 0; j < args_count; j++) {
            printf(" %d", code[i + 10 + j]);
        }
        printf("\n");
        return i + 10 + args_count;
    }
    case OP_CONST: {
        uint64_t c = read_u64(code, i + 1);
        uint8_t res = code[i + 9];
        printf("const %lu - %d\n", c, res);
        return i + 10;
    }
    case OP_LOAD: {
        uint64_t mem = read_u64(code, i + 1);
        uint8_t res = code[i + 9];
        printf("load %ld - %d\n", mem, res);
        return i + 10;
    }
    case OP_STORE: {
        uint64_t mem = read_u64(code, ++i);
        i += 8;
        uint8_t res = code[i];
        printf("store %ld - %d\n", mem, res);
        return i + 1;
    }
    case OP_CAST: {
        uint8_t reg = code[i + 1];
        uint8_t fromty = code[i + 2];
        uint8_t toty = code[i + 3];
        uint8_t res = code[i + 4];
        printf("cast %d %d %d - %d\n", reg, fromty, toty, res);
        return i + 5;
    }
    case OP_MOVE: {
        uint8_t res = code[i + 1];
        uint8_t r1 = code[i + 2];
        printf("move %d - %d\n", r1, res);
        return i + 3;
    }
    case OP_RETURN: {
        uint8_t r1 = code[i + 1];
        printf("return %d\n", r1);
        return i + 2;
    }
    case OP_POP: {
        uint8_t r1 = code[i + 1];
        printf("pop %d\n", r1);
        return i + 2;
    }
    case OP_PUSH: {
        uint8_t r1 = code[i + 1];
        printf("push %d\n", r1);
        return i + 2;
    }
    case OP_SPAWN: {
        printf("spawn\n");
        return i + 1;
    }
    case OP_STACKMAP: {
        uint64_t len = read_u64(code, i + 1);
        i += 9;
        printf("stackmap %ld\n", len);
        uint64_t *bitptr = malloc(sizeof(uint64_t) * len);
        while (len--) {
            *bitptr = read_u64(code, i);
            i += 8;
            print_bits(*bitptr);
            bitptr++;
        }
        return i;
    }
    case OP_PRINTLOCK: {
        printf("printlock\n");
        return i + 1;
    }
    case OP_PRINTUNLOCK: {
        printf("printunlock\n");
        return i + 1;
    }
    case OP_REF: 
    case OP_DEREF: 
    case OP_DEREF_ASSIGN: {
        uint64_t var  = read_u64(code, i + 1);
        uint8_t res = code[i + 9];
        if (code[i] == OP_REF) {
            printf("ref");
        } else  if (code[i] == OP_DEREF_ASSIGN) {
            printf("deref assgn");
        } else {
            printf("deref");
        }
        printf(" %ld - %d\n", var, res);
        return i + 10;
    }
    default:
        printf("Unknown opcode %d\n", code[i]);
        return i + 1;
    }
    return i;
}

void dissassemble(uint8_t *code, int length) {
    int i = 0;
    printf("i: %d\n", i);
    int start_ = read_u64(code, i);
    printf("start: %d\n", start_);
    i += 8;
    puts("strings:");
    int strings_count = read_u64(code, i);
    printf("string count: %d\n", strings_count);
    i += 8;
    while (strings_count-- && i < length) {
        uint64_t len = read_u64(code, i);
        i += 8;
        char *str = read_string(code, i, len);
        printf("\t%ld ", len);
        printf("%s\n", str);
        i += len;
        free(str);
    }
    // read func count
    // read func length
    // then read those bytes and print opcode
    int func_count = read_u64(code, i);
    i += 8;
    for (int j = 0; j < func_count; j++) {
        int func_length = read_u64(code, i);
        i += 8;
        // int argc = read_u64(code, i);
        // i += 8;
        // printf("func %d - length %d - argc %d\n", j, func_length, argc);
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