#include "opcodes.h"
#include <cstdint>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <sys/types.h>
#include <vector>

#pragma once

enum OpCodeType {
    OpcodeReg1U64Type,
    OpcodeReg2Type,
    OpcodeReg3Type,
    OpcodeReg1Type,
    OpcodeU64Type,
};

class Opcode {
public:
    Opcode() : opcode(-1) {}
    Opcode(uint8_t opcode);

    virtual void parse(std::istream &input) = 0;
    virtual std::string to_string() = 0;
    ~Opcode(){};

protected:
    uint8_t opcode;
};

std::vector<std::vector<Opcode *>> parse_binary(std::istream &input);

OpCodeType getOpCodeType(uint8_t opcode);

#define N_REG_OPCODE(name, opcode, n)                                          \
    class name : public Opcode {                                               \
    public:                                                                    \
        name() : Opcode(opcode) {}                                             \
        void parse(std::istream &input);                                       \
        std::string to_string();                                               \
                                                                               \
    protected:                                                                 \
        uint8_t reg[n];                                                        \
    };

#define THREE_REG_OPCODE(name, opcode) N_REG_OPCODE(name, opcode, 3)

#define TWO_REG_OPCODE(name, opcode) N_REG_OPCODE(name, opcode, 2)

#define CALL_OPCODE(name, opcode)                                              \
    class name : public Opcode {                                               \
    public:                                                                    \
        name() : Opcode(opcode) {}                                             \
        void parse(std::istream &input);                                       \
        std::string to_string();                                               \
                                                                               \
    protected:                                                                 \
        uint64_t fnix;                                                         \
        std::vector<uint8_t> args;                                             \
    };

#define CHAN_OPCODE(name, opcode)                                              \
    class name : public Opcode {                                               \
    public:                                                                    \
        name() : Opcode(opcode) {}                                             \
        void parse(std::istream &input);                                       \
        std::string to_string();                                               \
                                                                               \
    protected:                                                                 \
        uint64_t chan_var;                                                     \
        uint8_t reg;                                                           \
    };

#define ONE_REG_OPCODE(name, opcode)                                           \
    class name : public Opcode {                                               \
    public:                                                                    \
        name() : Opcode(opcode) {}                                             \
        void parse(std::istream &input);                                       \
        std::string to_string();                                               \
                                                                               \
    protected:                                                                 \
        uint8_t reg;                                                           \
    };

#define READ_U8_ARG(ptr)                                                       \
    input.read(reinterpret_cast<char *>(ptr), sizeof(uint8_t));

#define READ_U64_ARG(ptr)                                                      \
    input.read(reinterpret_cast<char *>(ptr), sizeof(uint64_t));

#define N_ARGS_OP_PARSE(name, n)                                               \
    void name::parse(std::istream &input) {                                    \
        for (uint8_t i = 0; i < n; i++) {                                      \
            READ_U8_ARG(reg + i)                                               \
        }                                                                      \
    }

#define CALL_OP_PARSE(name)                                                    \
    void name::parse(std::istream &input) {                                    \
        READ_U64_ARG(&fnix)                                                    \
        uint64_t count;                                                        \
        READ_U64_ARG(&count)                                                   \
        for (uint64_t i = 0; i < count; i++) {                                 \
            uint8_t arg;                                                       \
            READ_U8_ARG(&arg)                                                  \
            args.push_back(arg);                                               \
        }                                                                      \
    }

#define CHAN_OP_PARSE(name)                                                    \
    void name::parse(std::istream &input) {                                    \
        READ_U64_ARG(chan_var)                                                 \
        READ_U8_ARG(reg)                                                       \
    }

#define ONE_REG_OP_PARSE(name)                                                 \
    void name::parse(std::istream &input) { READ_U8_ARG(&reg) }

#define READ_U64(name)                                                         \
    uint64_t name;                                                             \
    input.read(reinterpret_cast<char *>(&name), sizeof(uint64_t));

#define THREE_ARGS_OP_PARSE(name) N_ARGS_OP_PARSE(name, 3)
#define TWO_ARGS_OP_PARSE(name) N_ARGS_OP_PARSE(name, 2)

#define PRINT_3_REG_OPCODE(name, opcode_name)                                  \
    std::string name::to_string() {                                            \
        std::stringstream ss;                                                  \
        ss << opcode_name << " " << (int)reg[0] << " " << (int)reg[1] << " = " \
           << (int)reg[2];                                                     \
        return ss.str();                                                       \
    }

#define PRINT_2_REG_OPCODE(name, opcode_name)                                  \
    std::string name::to_string() {                                            \
        std::stringstream ss;                                                  \
        ss << opcode_name << " " << (int)reg[0] << " = " << (int)reg[1];       \
        return ss.str();                                                       \
    }

#define PRINT_CALL_OPCODE(name, opcode_name)                                   \
    std::string name::to_string() {                                            \
        std::stringstream ss;                                                  \
        ss << opcode_name << " " << fnix << " (";                              \
        for (auto arg : args) {                                                \
            ss << (int)arg << " ";                                             \
        }                                                                      \
        ss << ")";                                                             \
        return ss.str();                                                       \
    }

#define PRINT_CHAN_OPCODE(name, opcode_name)                                   \
    std::string name::to_string() {                                            \
        std::stringstream ss;                                                  \
        ss << opcode_name << " " << chan_var << " " << (int)reg;               \
        return ss.str();                                                       \
    }

#define PRINT_1_REG_OPCODE(name, opcode_name)                                  \
    std::string name::to_string() {                                            \
        std::stringstream ss;                                                  \
        ss << opcode_name << " " << (int)reg;                                  \
        return ss.str();                                                       \
    };

THREE_REG_OPCODE(OpIAdd, OP_IADD)
THREE_REG_OPCODE(OpISub, OP_ISUB)
THREE_REG_OPCODE(OpIMul, OP_IMUL)
THREE_REG_OPCODE(OpIDiv, OP_IDIV)
THREE_REG_OPCODE(OpIRem, OP_IREM)
THREE_REG_OPCODE(OpINe, OP_INE)
THREE_REG_OPCODE(OpIEq, OP_IEQ)
THREE_REG_OPCODE(OpILt, OP_ILT)
THREE_REG_OPCODE(OpILe, OP_ILE)
THREE_REG_OPCODE(OpIGt, OP_IGT)
THREE_REG_OPCODE(OpIGe, OP_IGE)
THREE_REG_OPCODE(OpFAdd, OP_FADD)
THREE_REG_OPCODE(OpFSub, OP_FSUB)
THREE_REG_OPCODE(OpFMul, OP_FMUL)
THREE_REG_OPCODE(OpFDiv, OP_FDIV)
THREE_REG_OPCODE(OpFRem, OP_FREM)
THREE_REG_OPCODE(OpFNe, OP_FNE)
THREE_REG_OPCODE(OpFEq, OP_FEQ)
THREE_REG_OPCODE(OpFLt, OP_FLT)
THREE_REG_OPCODE(OpFLe, OP_FLE)
THREE_REG_OPCODE(OpFGt, OP_FGT)
THREE_REG_OPCODE(OpFGe, OP_FGE)

// from band to bnot
THREE_REG_OPCODE(OpBAnd, OP_BAND)
THREE_REG_OPCODE(OpBOr, OP_BOR)
THREE_REG_OPCODE(OpBXor, OP_BXOR)
TWO_REG_OPCODE(OpBNot, OP_BNOT)
THREE_REG_OPCODE(OpLAnd, OP_LAND)
THREE_REG_OPCODE(OpLOr, OP_LOR)
TWO_REG_OPCODE(OpLNot, OP_LNOT)
THREE_REG_OPCODE(OpBShl, OP_BSHL)
THREE_REG_OPCODE(OpBShr, OP_BSHR)

// from fneg to ineg
TWO_REG_OPCODE(OpFNeg, OP_FNEG)
TWO_REG_OPCODE(OpINeg, OP_INEG)

ONE_REG_OPCODE(OpReturn, OP_RETURN)
ONE_REG_OPCODE(OpPush, OP_PUSH)
ONE_REG_OPCODE(OpPop, OP_POP)

// from make to tuple
THREE_REG_OPCODE(OpListSet, OP_LISTSET)
THREE_REG_OPCODE(OpListGet, OP_LISTGET)
THREE_REG_OPCODE(OpTupleSet, OP_TUPLESET)
THREE_REG_OPCODE(OpTupleGet, OP_TUPLEGET)
THREE_REG_OPCODE(OpStrGet, OP_STRGET)
THREE_REG_OPCODE(OpMapGet, OP_MAPGET)
THREE_REG_OPCODE(OpMapSet, OP_MAPSET)

TWO_REG_OPCODE(OpMake, OP_MAKE)
TWO_REG_OPCODE(OpMove, OP_MOVE)

CALL_OPCODE(OpCall, OP_CALL)
CALL_OPCODE(OpNCall, OP_NCALL)
CALL_OPCODE(OpCoroCall, OP_CORO_CALL)

CHAN_OPCODE(OpChanSend, OP_CHANSEND)
CHAN_OPCODE(OpChanRecv, OP_CHANRECV)

class OpLoad : public Opcode {
public:
    OpLoad() : Opcode(OP_LOAD) {}
    void parse(std::istream &input);
    std::string to_string();

protected:
    uint8_t reg;
    uint64_t var_ix;
};

class OpStore : public Opcode {
public:
    OpStore() : Opcode(OP_STORE) {}
    void parse(std::istream &input);
    std::string to_string();

protected:
    uint8_t reg;
    uint64_t var_ix;
};

class OpJmp : public Opcode {
public:
    OpJmp() : Opcode(OP_JMP) {}
    void parse(std::istream &input);
    std::string to_string();

protected:
    uint64_t label;
};

class OpJmpNot : public Opcode {
public:
    OpJmpNot() : Opcode(OP_JMPNOT) {}
    void parse(std::istream &input);
    std::string to_string();

protected:
    uint8_t reg;
    uint64_t label;
};

class OpTuple : public Opcode {
public:
    OpTuple() : Opcode(OP_TUPLE) {}
    void parse(std::istream &input);
    std::string to_string();

protected:
    uint8_t reg;
    uint64_t count;
    std::vector<uint8_t> args;
    std::vector<uint64_t> type_bitsets;
};

class OpConst : public Opcode {
public:
    OpConst() : Opcode(OP_CONST) {}
    void parse(std::istream &input);
    std::string to_string();

protected:
    uint8_t reg;
    uint64_t const_ix;
};

class OpCast : public Opcode {
public:
    OpCast() : Opcode(OP_CAST) {}
    void parse(std::istream &input);
    std::string to_string();

protected:
    uint8_t res;
    uint8_t reg;
    uint64_t from_type_bitset;
    uint64_t to_type_bitset;
};

class OpNop : public Opcode {
public:
    OpNop() : Opcode(OP_NOP) {}
    void parse(std::istream &input);
    std::string to_string();
};

class OpRet : public Opcode {
public:
    OpRet() : Opcode(OP_RET) {}
    void parse(std::istream &input);
    std::string to_string();
};

class OpStackMap : public Opcode {
public:
    OpStackMap() : Opcode(OP_STACKMAP) {}
    void parse(std::istream &input);
    std::string to_string();

protected:
    std::vector<uint64_t> types;
};

// do the same using constexpr
constexpr const uint8_t opcodes[] = {
    OP_IADD,         OP_ISUB,        OP_IMUL,
    OP_IDIV,         OP_IREM,        OP_INE,
    OP_IEQ,          OP_ILT,         OP_ILE,
    OP_IGT,          OP_IGE,         OP_FADD,
    OP_FSUB,         OP_FMUL,        OP_FDIV,
    OP_FREM,         OP_FNE,         OP_FEQ,
    OP_FLT,          OP_FLE,         OP_FGT,
    OP_FGE,          OP_BAND,        OP_BOR,
    OP_BXOR,         OP_BNOT,        OP_LAND,
    OP_LOR,          OP_LNOT,        OP_BSHL,
    OP_BSHR,         OP_FNEG,        OP_INEG,
    OP_MAKE,         OP_LISTSET,     OP_LISTGET,
    OP_TUPLESET,     OP_TUPLEGET,    OP_TUPLE,
    OP_STRGET,       OP_MAPGET,      OP_MAPSET,
    OP_CHANSEND,     OP_CHANRECV,    OP_JMP,
    OP_JMPNOT,       OP_CALL,        OP_NCALL,
    OP_CONST,        OP_LOAD,        OP_STORE,
    OP_CAST,         OP_MOVE,        OP_RETURN,
    OP_PUSH,         OP_POP,         OP_SPAWN,
    OP_NOP,          OP_RET,         OP_STACKMAP,
    OP_PRINTLOCK,    OP_PRINTUNLOCK, OP_SUPER_INSTRUCTION,
    OP_CORO_CALL,    OP_REF,         OP_DEREF,
    OP_DEREF_ASSIGN,
};
