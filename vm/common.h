
#ifndef TYPES_H

#define TYPES_H

#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*1.5)

typedef uint64_t Value;

struct Queue {
    int capacity;
    int length;
    Value *items;
};

typedef struct Queue Queue;

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

enum ObjType { OBJ_STRING, OBJ_LIST, OBJ_TUPLE, OBJ_CHAN };

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
    int coro_count;
    int coro_id;
    pthread_t *threads;
    bool *coro_done;
};

typedef struct VM VM;

#endif