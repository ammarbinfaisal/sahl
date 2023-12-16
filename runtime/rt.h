#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>

#define GC_THREADS

#include "gc.h"
#include "gc/gc.h"

#ifndef H_RT

#define H_RT

struct Node {
    void *value;
    struct Node *next;
};

typedef struct Node Node;

struct LinkedList {
    int size;
    struct Node *head;
    struct Node *tail;
};

typedef struct LinkedList LinkedList;

enum Color { RED, BLACK };

struct RBNode {
    int64_t key;
    int64_t value;
    int color;
    struct RBNode *left;
    struct RBNode *right;
    struct RBNode *parent;
};

typedef struct RBNode RBNode;

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

typedef struct Chan chan_t;

enum ChanStatus { CHAN_OK, CHAN_CLOSED };
struct str_t {
    int64_t len;
    int constant;
    char *ptr;
};

typedef struct str_t str_t;

struct list_t {
    size_t cap;
    size_t length;
    uint64_t *data;
};

typedef struct list_t list_t;

enum ObjType { OBJ_STR, OBJ_LIST, OBJ_CHAN, OBJ_MAP };

typedef enum ObjType ObjType;

struct Obj {
    ObjType type;
    int marked;
    union {
        str_t *str;
        list_t *list;
        chan_t *chan;
        RBNode *map;
    };
};

typedef struct Obj Obj;

struct variant_t {
    uint64_t tag;
    uint64_t val;
};

typedef struct variant_t variant_t;

void iprint(int64_t);
void fprint(double);
void cprint(char);
void bprint(int);
void exit_with(int32_t);

variant_t *make_variant(uint64_t, uint64_t);
int is_variant(variant_t *, uint64_t);
int64_t get_variant(variant_t *);

Obj *newobj(ObjType);
Obj *make_string(char *, int);
void sprint(Obj *);
Obj *strcatt(Obj *, Obj *);
void str_free(str_t *);
char strget(Obj *, uint64_t);

int64_t mapget(Obj *, int64_t);
void mapset(Obj *, int64_t, int64_t);

Obj *make_list(size_t);
Obj *make_map();
Obj *make_chan(size_t);
typedef Obj *(*MakeFn)(size_t);
Obj *make(int, size_t);

void append(Obj *, int64_t);
uint64_t pop(Obj *);
void listset(Obj *, uint64_t, int64_t);
int64_t listget(Obj *, uint64_t);
void list_set_all(Obj *, void *);
void list_free(list_t *);
int len(Obj *);

Obj *concat(Obj *, Obj *);

void chansend(Obj *, uint64_t);
uint64_t chanrecv(Obj *);

void spawn(void *, void *);

#endif