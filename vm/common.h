
#ifndef TYPES_H

#define TYPES_H

#include "list.h"
#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>

// #define PRINT_OPCODES
// #define PRINT_STACK
// #define PRINT_LOCALS
// #define DEBUG
// #define DEBUGGC
// #define MINARR
#define UNSAFE
#define MAX_THREADS 16
#define USE_GC

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*1.5)

#define AS_DOUBLE(x) *(double *)&x

typedef uint64_t Value;

struct Queue {
    int capacity;
    int length;
    Value *items;
};

typedef struct Queue Queue;

struct RingBuffer {
    int capacity;
    int length;
    int head;
    int tail;
    Value *items;
};

typedef struct RingBuffer RingBuffer;

struct Chan {
    LinkedList *q;
    int len;
    // channel properties
    pthread_mutex_t m_mu;
    pthread_cond_t r_cond;
    pthread_cond_t w_cond;
    int closed;
    int r_waiting;
    int cap;
};

typedef struct Chan Chan;

struct Func {
    uint8_t *code;
    int code_length;
    int args_count;
};

typedef struct Func Func;

struct StackMap {
    int len;
    uint64_t *bits;
};

typedef struct StackMap StackMap;

struct CallFrame {
    uint32_t ip;
    Func *func;
    int locals_count;
    int locals_capacity;
    Value *locals;
    struct CallFrame *prev;
    struct CallFrame *next;
    StackMap *stackmap;
    int depth;
};

typedef struct CallFrame CallFrame;

struct RBNode {
    Value key;
    Value value;
    int color;
    struct RBNode *left;
    struct RBNode *right;
    struct RBNode *parent;
};

typedef struct RBNode RBNode;

enum ObjType { OBJ_STRING, OBJ_LIST, OBJ_TUPLE, OBJ_CHAN, OBJ_MAP };

typedef enum ObjType ObjType;

struct Obj {
    bool marked;
    ObjType type;
    struct Obj *next;
    union {
        struct {
            char *data;
            bool constant;
        } string;
        struct {
            uint64_t capacity;
            uint64_t length;
            Value *items;
            bool boxed_items;
        } list;
        struct {
            uint64_t length;
            Value *items;
            uint64_t* boxed_items;
        } tuple;
        struct {
            CallFrame *frame;
        } closure;
        struct {
            Chan *chan;
            bool boxed_items;
        } channel;
        struct {
            RBNode *map;
            bool key_boxed;
            bool value_boxed;
        } map;
    };
};

typedef struct Obj Obj;

union Reg {
    int64_t i;
    double f;
    Obj *o;
};

typedef union Reg Reg;

struct GCState {
    Obj *objects;
    int grayCount;
    int grayCapacity;
    Obj **grayStack;
    uint64_t allocated;
    uint64_t nextGC;
    pthread_mutex_t lock;
    pthread_mutex_t sweeplock;
};

typedef struct GCState GCState;

struct VM {
    Value *stack;
    int stack_size;
    Reg *regs; // 256 regs
    Func *funcs;
    int funcs_count;
    int string_count;
    Value *consts;
    uint32_t consts_count;
    CallFrame *call_frame;
    int start_func;

    // garbage collection
    GCState *gc_state;

    // thread
    bool coro_to_be_spawned;
    bool is_coro;
    int coro_id;
    bool should_yield;
    bool halted;

    char *filename;
};

typedef struct VM VM;

void error(VM *vm, char *msg);
char *stringify(Value value);

#endif