#include "code.h"
#include "consts.h"
#include "opcodes.h"

#include <cstdint>
#include <ios>
#include <iostream>
#include <sstream>
#include <stdint.h>
#include <string>
#include <vector>

/*Parse a 1-argument opcode from the input stream*/

OpCodeType getOpCodeType(uint8_t opcode) {
    switch (opcode) {
    case OP_IADD:
    case OP_ISUB:
    case OP_IMUL:
    case OP_IDIV:
    case OP_IREM:
    case OP_INE:
    case OP_IEQ:
    case OP_ILT:
    case OP_ILE:
    case OP_IGT:
    case OP_IGE:
    case OP_FADD:
    case OP_FSUB:
    case OP_FMUL:
    case OP_FDIV:
    case OP_FREM:
    case OP_FNE:
    case OP_FEQ:
    case OP_FLT:
    case OP_FLE:
    case OP_FGT:
    case OP_FGE:
    case OP_BAND:
    case OP_BOR:
    case OP_BXOR:
    case OP_BNOT:
    case OP_LAND:
    case OP_LOR:
    case OP_LNOT:
    case OP_BSHL:
    case OP_BSHR:
    case OP_LISTSET:
    case OP_LISTGET:
    case OP_TUPLESET:
    case OP_TUPLEGET:
    case OP_STRGET:
    case OP_MAPGET:
    case OP_MAPSET:
        return OpcodeReg3Type;
    case OP_FNEG:
    case OP_INEG:
    case OP_MAKE:
    case OP_MOVE:
        return OpcodeReg2Type;
    case OP_CHANSEND:
    case OP_CHANRECV:
    case OP_JMPNOT:
    case OP_CONST:
    case OP_LOAD:
    case OP_STORE:
        return OpcodeReg1U64Type;
    case OP_JMP:
    case OP_CALL:
    case OP_NCALL:
    case OP_RETURN:
        return OpcodeU64Type;
    case OP_PUSH:
    case OP_POP:
        return OpcodeReg1Type;
    default:
        std::cout << "Unknown opcode: " << opcode << std::endl;
    }
    return OpcodeReg3Type;
}

Opcode::Opcode(uint8_t opcode) : opcode(opcode) {}

Opcode *new_opcode(uint8_t opcode) {
    switch (opcode) {
    case OP_IADD:
        return new OpIAdd();
    case OP_ISUB:
        return new OpISub();
    case OP_IMUL:
        return new OpIMul();
    case OP_IDIV:
        return new OpIDiv();
    }

    return nullptr;
}

template <typename T> constexpr Opcode *construct() {
    return (Opcode *)(new T());
}

Opcode *parse_opcode(uint64_t op) {
    switch (op) {
    case OP_IADD:
        return new OpIAdd();
    case OP_ISUB:
        return new OpISub();
    case OP_IMUL:
        return new OpIMul();
    case OP_IDIV:
        return new OpIDiv();
    case OP_IREM:
        return new OpIRem();
    case OP_INE:
        return new OpINe();
    case OP_IEQ:
        return new OpIEq();
    case OP_ILT:
        return new OpILt();
    case OP_ILE:
        return new OpILe();
    case OP_IGT:
        return new OpIGt();
    case OP_IGE:
        return new OpIGe();
    case OP_FADD:
        return new OpFAdd();
    case OP_FSUB:
        return new OpFSub();
    case OP_FMUL:
        return new OpFMul();
    case OP_FDIV:
        return new OpFDiv();
    case OP_FREM:
        return new OpFRem();
    case OP_FNE:
        return new OpFNe();
    case OP_FEQ:
        return new OpFEq();
    case OP_FLT:
        return new OpFLt();
    case OP_FLE:
        return new OpFLe();
    case OP_FGT:
        return new OpFGt();
    case OP_FGE:
        return new OpFGe();
    case OP_BAND:
        return new OpBAnd();
    case OP_BOR:
        return new OpBOr();
    case OP_BXOR:
        return new OpBXor();
    case OP_BNOT:
        return new OpBNot();
    case OP_LAND:
        return new OpLAnd();
    case OP_LOR:
        return new OpLOr();
    case OP_LNOT:
        return new OpLNot();
    case OP_BSHL:
        return new OpBShl();
    case OP_BSHR:
        return new OpBShr();
    case OP_FNEG:
        return new OpFNeg();
    case OP_INEG:
        return new OpINeg();
    case OP_MAKE:
        return new OpMake();
    case OP_LISTSET:
        return new OpListSet();
    case OP_LISTGET:
        return new OpListGet();
    case OP_TUPLESET:
        return new OpTupleSet();
    case OP_TUPLEGET:
        return new OpTupleGet();
    case OP_TUPLE:
        return new OpTuple();
    case OP_STRGET:
        return new OpStrGet();
    case OP_MAPGET:
        return new OpMapGet();
    case OP_MAPSET:
        return new OpMapSet();
    case OP_CHANSEND:
        return new OpChanSend();
    case OP_CHANRECV:
        return new OpChanRecv();
    case OP_JMP:
        return new OpJmp();
    case OP_JMPNOT:
        return new OpJmpNot();
    case OP_CALL:
        return new OpCall();
    case OP_NCALL:
        return new OpNCall();
    case OP_CONST:
        return new OpConst();
    case OP_LOAD:
        return new OpLoad();
    case OP_STORE:
        return new OpStore();
    case OP_CAST:
        return new OpCast();
    case OP_MOVE:
        return new OpMove();
    case OP_RETURN:
        return new OpReturn();
    case OP_PUSH:
        return new OpPush();
    case OP_POP:
        return new OpPop();
    case OP_NOP:
        return new OpNop();
    case OP_RET:
        return new OpRet();
    case OP_STACKMAP:
        return new OpStackMap();
    case OP_CORO_CALL:
        return new OpCoroCall();
    }
    std::cout << "Unknown opcode: " << op << std::endl;
    exit(1);
}

std::vector<std::vector<Opcode *>> parse_binary(std::istream &input) {
    std::vector<std::vector<Opcode *>> result;
    uint64_t start_func, const_count;
    std::vector<Const *> consts;
    input.read(reinterpret_cast<char *>(&start_func), sizeof(uint64_t));
    input.read(reinterpret_cast<char *>(&const_count), sizeof(uint64_t));
    std::cout << "Start func: " << start_func << std::endl;
    std::cout << "Const count: " << const_count << std::endl;

    for (int i = 0; i < const_count; i++) {
        uint8_t ty;
        input.read(reinterpret_cast<char *>(&ty), sizeof(uint8_t));
        Const *cval;
        switch (ty) {
        case 0:
            cval = new ConstInt();
            break;
        case 1:
            cval = new ConstFloat();
            break;
        case 2:
            cval = new ConstBool();
            break;
        case 3:
            cval = new ConstChar();
            break;
        case 4:
            cval = new ConstString();
            break;
        }
        cval->parse(input);
        consts.push_back(cval);
        std::cout << "Const: " << cval->to_string() << std::endl;
    }

    uint64_t func_count;
    input.read(reinterpret_cast<char *>(&func_count), sizeof(uint64_t));

    for (int i = 0; i < func_count; i++) {
        uint64_t func_len;
        input.read(reinterpret_cast<char *>(&func_len), sizeof(uint64_t));

        std::vector<Opcode *> func;
        for (int j = input.tellg(); input.tellg() < j + func_len;) {
            uint8_t opcode;
            input.read(reinterpret_cast<char *>(&opcode), sizeof(uint8_t));
            Opcode *op = parse_opcode(opcode);
            op->parse(input);
            func.push_back(op);
            std::cout << op->to_string() << std::endl;
        }
        result.push_back(func);
    }
    return result;
}

void OpJmp::parse(std::istream &input) { READ_U64_ARG(&label) }

void OpTuple::parse(std::istream &input) {
    READ_U64_ARG(&count)
    int types_len = 0;
    READ_U64_ARG(&types_len)
    for (int i = 0; i < types_len; i++) {
        uint64_t type;
        READ_U64_ARG(&type)
        type_bitsets.push_back(type);
    }
    READ_U8_ARG(&reg)
}

void OpJmpNot::parse(std::istream &input) {
    READ_U8_ARG(&reg)
    READ_U64_ARG(&label)
}

void OpConst::parse(std::istream &input) {
    READ_U64_ARG(&const_ix)
    READ_U8_ARG(&reg)
}

void OpCast::parse(std::istream &input) {
    READ_U64_ARG(&res)
    READ_U64_ARG(&reg)
    READ_U64_ARG(&from_type_bitset)
    READ_U64_ARG(&to_type_bitset)
}

void OpNop::parse(std::istream &input) {}

void OpRet::parse(std::istream &input) {}

void OpStackMap::parse(std::istream &input) {
    uint64_t bitsets_len;
    READ_U64_ARG(&bitsets_len)
    for (int i = 0; i < bitsets_len; i++) {
        uint64_t bitset;
        READ_U64_ARG(&bitset)
        types.push_back(bitset);
    }
}

THREE_ARGS_OP_PARSE(OpIAdd)
THREE_ARGS_OP_PARSE(OpISub)
THREE_ARGS_OP_PARSE(OpIMul)
THREE_ARGS_OP_PARSE(OpIDiv)
THREE_ARGS_OP_PARSE(OpIRem)
THREE_ARGS_OP_PARSE(OpINe)
THREE_ARGS_OP_PARSE(OpIEq)
THREE_ARGS_OP_PARSE(OpILt)
THREE_ARGS_OP_PARSE(OpILe)
THREE_ARGS_OP_PARSE(OpIGt)
THREE_ARGS_OP_PARSE(OpIGe)
THREE_ARGS_OP_PARSE(OpFAdd)
THREE_ARGS_OP_PARSE(OpFSub)
THREE_ARGS_OP_PARSE(OpFMul)
THREE_ARGS_OP_PARSE(OpFDiv)
THREE_ARGS_OP_PARSE(OpFRem)
THREE_ARGS_OP_PARSE(OpFNe)
THREE_ARGS_OP_PARSE(OpFEq)
THREE_ARGS_OP_PARSE(OpFLt)
THREE_ARGS_OP_PARSE(OpFLe)
THREE_ARGS_OP_PARSE(OpFGt)
THREE_ARGS_OP_PARSE(OpFGe)
THREE_ARGS_OP_PARSE(OpBAnd)
THREE_ARGS_OP_PARSE(OpBOr)
THREE_ARGS_OP_PARSE(OpBXor)
THREE_ARGS_OP_PARSE(OpBNot)
THREE_ARGS_OP_PARSE(OpLAnd)
THREE_ARGS_OP_PARSE(OpLOr)
THREE_ARGS_OP_PARSE(OpLNot)
THREE_ARGS_OP_PARSE(OpBShl)
THREE_ARGS_OP_PARSE(OpBShr)
THREE_ARGS_OP_PARSE(OpListSet)
THREE_ARGS_OP_PARSE(OpListGet)
THREE_ARGS_OP_PARSE(OpTupleSet)
THREE_ARGS_OP_PARSE(OpTupleGet)
THREE_ARGS_OP_PARSE(OpStrGet)
THREE_ARGS_OP_PARSE(OpMapGet)
THREE_ARGS_OP_PARSE(OpMapSet)

/*Parse a 2-argument opcode from the input stream*/

TWO_ARGS_OP_PARSE(OpFNeg)
TWO_ARGS_OP_PARSE(OpINeg)
TWO_ARGS_OP_PARSE(OpMake)
TWO_ARGS_OP_PARSE(OpMove)

// from load to store
ONE_REG_OP_PARSE(OpReturn)
ONE_REG_OP_PARSE(OpPush)
ONE_REG_OP_PARSE(OpPop)

CALL_OP_PARSE(OpCall)
CALL_OP_PARSE(OpCoroCall)

CHAN_OP_PARSE(OpChanSend)
CHAN_OP_PARSE(OpChanRecv)

void OpNCall::parse(std::istream &input) {
    READ_U8_ARG(&fnix)
    uint64_t count;
    READ_U64_ARG(&count)
    for (int i = 0; i < count; i++) {
        uint8_t arg;
        READ_U8_ARG(&arg)
        args.push_back(arg);
    }
}

void OpLoad::parse(std::istream &input) {
    READ_U64_ARG(&var_ix)
    READ_U8_ARG(&reg)
}

void OpStore::parse(std::istream &input) {
    READ_U64_ARG(&var_ix)
    READ_U8_ARG(&reg)
}

PRINT_3_REG_OPCODE(OpIAdd, "iadd")
PRINT_3_REG_OPCODE(OpISub, "isub")
PRINT_3_REG_OPCODE(OpIMul, "imul")
PRINT_3_REG_OPCODE(OpIDiv, "idiv")
PRINT_3_REG_OPCODE(OpIRem, "irem")
PRINT_3_REG_OPCODE(OpINe, "ine")
PRINT_3_REG_OPCODE(OpIEq, "ieq")
PRINT_3_REG_OPCODE(OpILt, "ilt")
PRINT_3_REG_OPCODE(OpILe, "ile")
PRINT_3_REG_OPCODE(OpIGt, "igt")
PRINT_3_REG_OPCODE(OpIGe, "ige")
PRINT_3_REG_OPCODE(OpFAdd, "fadd")
PRINT_3_REG_OPCODE(OpFSub, "fsub")
PRINT_3_REG_OPCODE(OpFMul, "fmul")
PRINT_3_REG_OPCODE(OpFDiv, "fdiv")
PRINT_3_REG_OPCODE(OpFRem, "frem")
PRINT_3_REG_OPCODE(OpFNe, "fne")
PRINT_3_REG_OPCODE(OpFEq, "feq")
PRINT_3_REG_OPCODE(OpFLt, "flt")
PRINT_3_REG_OPCODE(OpFLe, "fle")
PRINT_3_REG_OPCODE(OpFGt, "fgt")
PRINT_3_REG_OPCODE(OpFGe, "fge")
PRINT_3_REG_OPCODE(OpBAnd, "band")
PRINT_3_REG_OPCODE(OpBOr, "bor")
PRINT_3_REG_OPCODE(OpBXor, "bxor")
PRINT_3_REG_OPCODE(OpLAnd, "land")
PRINT_3_REG_OPCODE(OpLOr, "lor")
PRINT_3_REG_OPCODE(OpBShl, "bshl")
PRINT_3_REG_OPCODE(OpBShr, "bshr")
PRINT_3_REG_OPCODE(OpListSet, "listset")
PRINT_3_REG_OPCODE(OpListGet, "listget")
PRINT_3_REG_OPCODE(OpTupleSet, "tupleset")
PRINT_3_REG_OPCODE(OpTupleGet, "tupleget")
PRINT_3_REG_OPCODE(OpStrGet, "strget")
PRINT_3_REG_OPCODE(OpMapGet, "mapget")
PRINT_3_REG_OPCODE(OpMapSet, "mapset")

PRINT_2_REG_OPCODE(OpBNot, "bnot")
PRINT_2_REG_OPCODE(OpLNot, "lnot")
PRINT_2_REG_OPCODE(OpFNeg, "fneg")
PRINT_2_REG_OPCODE(OpINeg, "ineg")
PRINT_2_REG_OPCODE(OpMake, "make")
PRINT_2_REG_OPCODE(OpMove, "move")

PRINT_CALL_OPCODE(OpCall, "call")
PRINT_CALL_OPCODE(OpNCall, "ncall")
PRINT_CALL_OPCODE(OpCoroCall, "corocall")

PRINT_CHAN_OPCODE(OpChanSend, "chansend")
PRINT_CHAN_OPCODE(OpChanRecv, "chanrecv")

PRINT_1_REG_OPCODE(OpReturn, "return")
PRINT_1_REG_OPCODE(OpPush, "push")
PRINT_1_REG_OPCODE(OpPop, "pop")

std::string OpJmp::to_string() { return "jmp " + std::to_string(label); }

std::string OpTuple::to_string() {
    std::stringstream ss;
    ss << "tuple " << (int)reg << " " << count << " ";
    for (auto &type : args) {
        ss << type << " ";
    }
    return ss.str();
}

std::string OpJmpNot::to_string() {
    std::stringstream ss;
    ss << "jmpnot " << reg << " " << label;
    return ss.str();
}

std::string OpConst::to_string() {
    std::stringstream ss;
    ss << "const " << (int)reg << " ix(" << const_ix << ")";
    return ss.str();
}

std::string OpCast::to_string() {
    std::stringstream ss;
    ss << "cast " << res << " " << reg << " " << from_type_bitset << " "
       << to_type_bitset;
    return ss.str();
}

std::string OpNop::to_string() { return "nop"; }

std::string OpRet::to_string() { return "ret"; }

std::string OpStackMap::to_string() {
    std::stringstream ss;
    ss << "stackmap ";
    ss << std::hex;
    for (auto &type : types) {
        ss << type << " ";
    }
    return ss.str();
}

std::string OpLoad::to_string() {
    std::stringstream ss;
    ss << "load " << (int)reg << " " << var_ix;
    return ss.str();
}

std::string OpStore::to_string() {
    std::stringstream ss;
    ss << "store " << (int)reg << " " << var_ix;
    return ss.str();
}
