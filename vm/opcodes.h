#ifndef OPCODES_H

#define OPCODES_H

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
#define NATIVE_CALL 37
#define CONST_DOUBLE 38
#define MAKE_CHAN 39
#define CHAN_READ 40
#define CHAN_WRITE 41
#define SPAWN 42
#define MAKE_MAP 43
#define NUM_OPCODES 44

#endif
