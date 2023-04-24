#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#define GROW_CAPACITY(cap) ((cap) < 8 ? 8 : (cap)*2)

#define NANISH 0x7FF8000000000000
#define NANISH_MASK 0xffff000000000000
#define OBJECT_MASK 0xfffc000000000000
#define OBJ_VAL(obj) (uint64_t)(OBJECT_MASK | (uint64_t)(obj))
#define AS_OBJ(value) ((Obj *)(((uint64_t)value) & 0xFFFFFFFFFFFF))
#define IS_OBJ(value) ((value & NANISH_MASK) == OBJECT_MASK)

// #define DEBUG

struct str_t {
    int64_t len;
    int constant;
    char *ptr;
};

typedef struct str_t str_t;

enum ObjType {
    OBJ_STR,
};

typedef enum ObjType ObjType;

struct Obj {
    ObjType type;
    struct Obj *next;
    int marked;
    union {
        str_t *str;
    };
};

typedef struct Obj Obj;

struct Heap {
    Obj *first;
    Obj *last;
};

typedef struct Heap Heap;

struct GrayStack {
    Obj **ptr;
    int64_t len;
    int64_t cap;
};

typedef struct GrayStack GrayStack;

// to keep track of the native stack
struct CallFrame {
    int64_t rbp;
    int64_t rsp;
};

typedef struct CallFrame CallFrame;

struct CallStack {
    CallFrame *ptr;
    struct CallStack *next;
};

typedef struct CallStack CallStack;

CallStack *call_stack = NULL;

Heap heap = {NULL, NULL};

GrayStack gray_stack = {NULL, 0, 0};

uint64_t alloced = 0;

uint64_t next_gc = 1024 * 1024;

// gc definitions

void sweep();

void mark_obj(Obj *obj);

void trace_roots();

void free_obj(Obj *obj);

void collect_garbage();

void trace_roots();

void register_call_frame(int64_t rbp, int64_t rsp);

void unregister_call_frame();

// end gc definitions

void iprint(int64_t i) { printf("%ld", i); }

void fprint(double f) { printf("%lf", f); }

void cprint(char c) { printf("%c", c); }

void bprint(int b) { printf("%s", b ? "true" : "false"); }

double ifadd(int64_t a, double b) { return a + b; }

double ffadd(double a, double b) { return a + b; }

double ifsub(int64_t a, double b) { return a - b; }

double ffsub(double a, double b) { return a - b; }

double ifmul(int64_t a, double b) { return a * b; }

double ffmul(double a, double b) { return a * b; }

int64_t iidiv(int64_t a, int64_t b) { return a / b; }

double ifdiv(int64_t a, double b) { return a / b; }

double fidiv(double a, int64_t b) { return a / b; }

double ffdiv(double a, double b) { return a / b; }

int ifcmp(int64_t a, double b) { return a < (int64_t)b ? -1 : a > b ? 1 : 0; }

int ffcmp(double a, double b) { return a < b ? -1 : a > b ? 1 : 0; }

void exit_with(int32_t code) { exit(code); }

void *allocate(int64_t size) {
    if (alloced + size > next_gc) {
        collect_garbage();
    }

    void *ptr = malloc(size);
    if (ptr == NULL) {
        printf("out of memory  \n");
        exit(1);
    }

    return ptr;
}

Obj *newobj(ObjType ty) {
    Obj *obj = (Obj *)allocate(sizeof(Obj));
    obj->marked = 0;
    obj->next = NULL;
    obj->type = ty;
#ifdef DEBUG
    printf("newobj(%d) = %p \n", ty, obj);
#endif
    if (heap.first == NULL) {
        heap.first = obj;
        heap.last = obj;
    } else {
        heap.last->next = obj;
        heap.last = obj;
    }
}

uint64_t newstr(char *ptr) {
    str_t *str = (str_t *)allocate(sizeof(str_t));
    str->len = strlen(ptr);
    str->ptr = ptr;
    str->constant = 1;
    Obj *obj = newobj(OBJ_STR);
    obj->str = str;
    if (heap.first == NULL) {
        heap.first = obj;
        heap.last = obj;
    } else {
        heap.last->next = obj;
        heap.last = obj;
    }

#ifdef DEBUG
    printf("newstr(%p) = %p (%s) \t val = (%p)\n", ptr, obj, ptr,
           (void *)OBJ_VAL(obj));
#endif
    return OBJ_VAL(obj);
}

void sprint(uint64_t s) {
    Obj *sobj = AS_OBJ(s);
    str_t *str = sobj->str;
    printf("%s", str->ptr);
}

uint64_t strcatt(uint64_t a, uint64_t b) {
#ifdef DEBUG
    printf("strcatt(%ld, %ld)\n", a, b);
#endif
    Obj *aobj = AS_OBJ(a);
    Obj *bobj = AS_OBJ(b);
#ifdef DEBUG
    printf("strcatt(%p, %p)\n", aobj, bobj);
#endif
    str_t *astr = aobj->str;
    str_t *bstr = aobj->str;
    int64_t len = astr->len + bstr->len;
    char *ptr = (char *)allocate(len + 1);
    memcpy(ptr, astr->ptr, astr->len);
    memcpy(ptr + astr->len, bstr->ptr, bstr->len);
    ptr[len] = '\0';
    str_t *str = (str_t *)allocate(sizeof(str_t));
    str->len = len;
    str->ptr = ptr;
    str->constant = 0;
    Obj *obj = newobj(OBJ_STR);
    obj->str = str;
    return OBJ_VAL(obj);
}

void str_free(str_t *str) {
    if (!str->constant) {
        free(str->ptr);
    }
    free(str);
}

void mark_obj(Obj *obj) {
    if (obj == NULL) return;
    if (obj->marked) return;
    printf("marking %p \t val = (%p) \t type = %d\n", obj, (void *)OBJ_VAL(obj),
           obj->type);
    obj->marked = 1;

    if (gray_stack.len + 1 > gray_stack.cap) {
        gray_stack.cap = gray_stack.cap == 0 ? 8 : gray_stack.cap * 2;
        gray_stack.ptr =
            (Obj **)realloc(gray_stack.ptr, sizeof(Obj *) * gray_stack.cap);
    }

    gray_stack.ptr[gray_stack.len++] = obj;
}

// used by list, tuple, map, and chan
void blacken_obj(Obj *obj) {
    switch (obj->type) {
    case OBJ_STR: {
        break;
    }
    }
}

void mark_roots() {
    // traverse the stack and mark all objects
    CallStack *frame = call_stack;
    while (frame != NULL) {
        int64_t rbp = frame->ptr->rbp;
        int64_t rsp = frame->ptr->rsp;
#ifdef DEBUG
        printf("rbp = %ld \t rsp = %ld\n", rbp, rsp);
#endif
        int64_t *ptr = (int64_t *)rbp;
        while (ptr < (int64_t *)rsp) {
            ptr--;
            if (IS_OBJ(*ptr)) {
                mark_obj(AS_OBJ(*ptr));
            }
        }
        frame = frame->next;
    }
}

void trace_roots() {
    while (gray_stack.len > 0) {
        Obj *obj = gray_stack.ptr[--gray_stack.len];
        blacken_obj(obj);
    }
}

void sweep() {
    Obj *prev = NULL;
    Obj *obj = heap.first;
    while (obj != NULL) {
        if (obj->marked) {
            obj->marked = 0;
            prev = obj;
            obj = obj->next;
        } else {
            Obj *next = obj->next;
            if (prev == NULL) {
                heap.first = next;
            } else {
                prev->next = next;
            }
            if (next == NULL) {
                heap.last = prev;
            }
#ifdef DEBUG
            printf("freeing %p \n", obj);
#endif
            free_obj(obj);
            obj = next;
        }
    }
}

void collect_garbage() {
    mark_roots();
    trace_roots();
    sweep();
    next_gc = alloced * 1.5;
}

void free_obj(Obj *obj) {
    switch (obj->type) {
    case OBJ_STR: {
        str_free(obj->str);
        break;
    }
    }
    free(obj);
}

void register_call_frame(int64_t rbp, int64_t rsp) {
    CallStack *frame = (CallStack *)malloc(sizeof(CallStack));
    frame->ptr = (CallFrame *)malloc(sizeof(CallFrame));
    frame->ptr->rbp = rbp;
    frame->ptr->rsp = rsp;
    frame->next = call_stack;
    call_stack = frame;
}

void unregister_call_frame() {
    CallStack *frame = call_stack;
    call_stack = call_stack->next;
    free(frame->ptr);
    free(frame);
}
