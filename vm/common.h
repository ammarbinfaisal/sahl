
#ifndef TYPES_H

#define TYPES_H

#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>

// #define PRINT_OPCODES
// #define PRINT_STACK
// #define PRINT_LOCALS
// #define DEBUG
// #define MINARR
#define UNSAFE

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*1.5)

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
    Queue *q;
    // channel properties
    pthread_mutex_t m_mu;
    pthread_cond_t r_cond;
    pthread_cond_t w_cond;
    int closed;
    int r_waiting;
    int w_waiting;
};

typedef struct Chan Chan;

struct Func {
    uint8_t *code;
    int code_length;
};

typedef struct Func Func;

struct CallFrame {
    uint32_t ip;
    Func *func;
    int locals_count;
    int locals_capacity;
    Value *locals;
    struct CallFrame *prev;
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
        } list;
        struct {
            uint64_t length;
            Value *items;
        } tuple;
        struct {
            CallFrame *frame;
        } closure;
        struct {
            Chan *chan;
        } channel;
        struct {
            RBNode *map;
        } map;
    };
};

typedef struct Obj Obj;

struct VM {
    Value *stack;
    int stack_size;
    Func *funcs;
    int funcs_count;
    int string_count;
    char **strings;
    CallFrame *call_frame;
    int start_func;

    // garbage collection
    Obj *objects;
    int grayCount;
    int grayCapacity;
    Obj **grayStack;
    uint64_t allocated;
    uint64_t nextGC;

    // thread
    bool coro_to_be_spawned;
    bool is_coro;
    int thread_count;
    int coro_id;
    pthread_t *threads;
    bool *coro_done;
};

typedef struct VM VM;

void error(VM *vm, char *msg);
char *stringify(Value value);

#endif