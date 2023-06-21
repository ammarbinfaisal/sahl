#ifndef OPCODES_H

#define OPCODES_H

#define OP_IADD 0
#define OP_ISUB 1
#define OP_IMUL 2
#define OP_IDIV 3
#define OP_IREM 4
#define OP_INE 5
#define OP_IEQ 6
#define OP_ILT 7
#define OP_ILE 8
#define OP_IGT 9
#define OP_IGE 10
#define OP_FADD 11
#define OP_FSUB 12
#define OP_FMUL 13
#define OP_FDIV 14
#define OP_FREM 15
#define OP_FNE 16
#define OP_FEQ 17
#define OP_FLT 18
#define OP_FLE 19
#define OP_FGT 20
#define OP_FGE 21
#define OP_BAND 22
#define OP_BOR 23
#define OP_BXOR 24
#define OP_BNOT 25
#define OP_LAND 26
#define OP_LOR 27
#define OP_LNOT 28
#define OP_BSHL 29
#define OP_BSHR 30
#define OP_FNEG 31
#define OP_INEG 32
#define OP_MAKE 33
#define OP_LISTSET 34
#define OP_LISTGET 35
#define OP_LIST 36
#define OP_TUPLEGET 37
#define OP_TUPLE 38
#define OP_STRGET 39
#define OP_MAPGET 40
#define OP_MAPSET 41
#define OP_CHANSEND 42
#define OP_CHANRECV 43
#define OP_JMP 44
#define OP_JMPNOT 45
#define OP_CALL 46
#define OP_NCALL 47
#define OP_CONST 48
#define OP_LOAD 49
#define OP_STORE 50
#define OP_CAST 51
#define OP_MOVE 52
#define OP_RETURN 53
#define OP_PUSH 54
#define OP_POP 55
#define OP_SPAWN 56
#define OP_NOP 57
#define OP_RET 58

#define NUM_OPCODES 64

#endif
