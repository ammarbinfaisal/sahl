#ifndef OPCODES_H

#define OPCODES_H

#define IADD 0
#define ISUB 1
#define IMUL 2
#define IDIV 3
#define IMOD 4
#define INEG 5
#define NOT 6
#define AND 7
#define OR 8
#define EQUAL 9
#define NOT_EQUAL 10
#define ILESS 11
#define ILESS_EQUAL 12
#define IGREATER 13
#define IGREATER_EQUAL 14
#define TRUE 15
#define FALSE 16
#define JUMP 17
#define JUMP_IF_FALSE 18
#define STORE 19
#define LIST_INDEX 20
#define LIST_APPEND 21
#define LIST_LENGTH 22
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
#define NATIVE_CALL 37
#define CONST_DOUBLE 38
#define MAKE_CHAN 39
#define CHAN_READ 40
#define CHAN_WRITE 41
#define SPAWN 42
#define MAKE_MAP 43
#define FADD 44
#define FSUB 45
#define FMUL 46
#define FDIV 47
#define FNEG 48
#define FLESS 49
#define FLESS_EQUAL 50
#define FGREATER 51
#define FGREATER_EQUAL 52
#define SCONCAT 53
#define I2F 54
#define I2S 55
#define F2S 56
#define FMOD 57
#define BIT_AND 58
#define BIT_OR 59
#define BIT_XOR 60
#define BIT_SHL 61
#define BIT_SHR 62

#define NUM_OPCODES 63

#endif
