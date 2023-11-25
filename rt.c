#include "gc.h"
#include <pthread.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

// #define DEBUG

#ifdef DEBUG
#define DEBUG_printf(...) printf(__VA_ARGS__)
#else
#define DEBUG_printf(...)
#endif

void sahl_main();

// List

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

LinkedList *new_list() {
    LinkedList *q;
    q = GC_malloc(sizeof(LinkedList));

    if (q == NULL) {
        return q;
    }

    q->size = 0;
    q->head = NULL;
    q->tail = NULL;

    return q;
}

int enqueue(LinkedList *q, void *value) {
    Node *node = GC_malloc(sizeof(Node));

    if (node == NULL) {
        return q->size;
    }

    node->value = value;
    node->next = NULL;

    if (q->head == NULL) {
        q->head = node;
        q->tail = node;
        q->size = 1;

        return q->size;
    }

    q->tail->next = node;
    q->tail = node;
    q->size += 1;

    return q->size;
}

void *dequeue(LinkedList *q) {
    if (q->size == 0) {
        return NULL;
    }

    void *value = NULL;
    struct Node *tmp = NULL;

    value = q->head->value;
    tmp = q->head;
    q->head = q->head->next;
    q->size -= 1;

    GC_free(tmp);

    return value;
}

void free_linkedlist(LinkedList *q) {
    if (q == NULL) {
        return;
    }

    while (q->head != NULL) {
        Node *tmp = q->head;
        q->head = q->head->next;
        if (tmp->value != NULL) {
            GC_free(tmp->value);
        }

        GC_free(tmp);
    }

    GC_free(q);
}

// Chan

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

chan_t *new_chan(int capacity) {
    chan_t *c = malloc(sizeof(chan_t));
    c->q = new_list();
    c->len = 0;
    c->cap = capacity;

    pthread_mutex_init(&c->m_mu, NULL);
    pthread_cond_init(&c->r_cond, NULL);
    pthread_cond_init(&c->w_cond, NULL);
    c->closed = 0;
    c->r_waiting = 0;

    return c;
}

void close_chan(chan_t *c) {
    pthread_mutex_lock(&c->m_mu);
    c->closed = 1;
    pthread_cond_broadcast(&c->r_cond);
    pthread_cond_broadcast(&c->w_cond);
    pthread_mutex_unlock(&c->m_mu);
}

static int chan_write(chan_t *chan, uint64_t v) {
    if (chan->closed) {
        return CHAN_CLOSED;
    }
    enqueue(chan->q, (void *)v);
    chan->len++;
    return CHAN_OK;
}

static int chan_read(chan_t *chan, uint64_t *v) {
    if (chan->closed) {
        return CHAN_CLOSED;
    }
    --chan->len;
    *v = (uint64_t)dequeue(chan->q);
    return CHAN_OK;
}

// Objects

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

enum ObjType { OBJ_STR, OBJ_LIST, OBJ_CHAN };

typedef enum ObjType ObjType;

struct Obj {
    ObjType type;
    int marked;
    union {
        str_t *str;
        list_t *list;
        chan_t *chan;
    };
};

typedef struct Obj Obj;

void iprint(int64_t i) { printf("%ld", i); }

void fprint(double f) { printf("%lf", f); }

void cprint(char c) { printf("%c", c); }

void bprint(int b) { printf("%s", b ? "true" : "false"); }

void exit_with(int32_t code) { exit(code); }

Obj *newobj(ObjType ty) {
    Obj *obj = (Obj *)GC_malloc(sizeof(Obj));
    obj->marked = 0;
    obj->type = ty;
    return obj;
}

Obj *make_string(char *ptr, int len) {
    str_t *str = (str_t *)GC_malloc(sizeof(str_t));
    str->len = len;
    str->ptr = ptr;
    str->constant = 1;
    Obj *obj = newobj(OBJ_STR);
    obj->str = str;

#ifdef DEBUG
    printf("newstr(%p) -> %p\n", ptr, obj);
#endif
    return obj;
}

void sprint(Obj *o) { printf("%s", o->str->ptr); }

Obj *strcatt(Obj *aobj, Obj *bobj) {
#ifdef DEBUG
    printf("strcatt(%p, %p)\n", aobj, bobj);
#endif
    str_t *astr = aobj->str;
    str_t *bstr = aobj->str;
    int64_t len = astr->len + bstr->len;
    char *ptr = (char *)GC_malloc(len + 1);
    memcpy(ptr, astr->ptr, astr->len);
    memcpy(ptr + astr->len, bstr->ptr, bstr->len);
    ptr[len] = '\0';
    str_t *str = (str_t *)GC_malloc(sizeof(str_t));
    str->len = len;
    str->ptr = ptr;
    str->constant = 0;
    Obj *obj = newobj(OBJ_STR);
    obj->str = str;
    return obj;
}

void str_free(str_t *str) {
    if (!str->constant) {
        GC_free(str->ptr);
    }
    GC_free(str);
}

Obj *make_list(size_t size) {
    list_t *list = (list_t *)GC_malloc(sizeof(list_t));
    list->cap = size;
    list->length = size;
    size_t newsize = size * sizeof(uint64_t);
    list->data = (uint64_t *)GC_malloc(newsize);
    Obj *obj = newobj(OBJ_LIST);
    obj->list = list;
    return obj;
}

Obj *make_chan(size_t size) {
    chan_t *chan = new_chan(size);
    Obj *obj = newobj(OBJ_CHAN);
    obj->chan = chan;
    return obj;
}

typedef Obj *(*MakeFn)(size_t len);

static MakeFn make_fns[] = {make_list, make_list, make_chan};

Obj *make(int ty, size_t size) {
    return make_fns[ty - 5](size);
}

void append(Obj *obj, int64_t val) {
    list_t *l = obj->list;
    if (l->length + 1 > l->cap) {
        l->cap = l->cap == 0 ? 8 : l->cap * 2;
        size_t newsize = l->cap * sizeof(uint64_t);

        l->data = (uint64_t *)GC_realloc(l->data, newsize);
    }
    l->data[l->length] = val;
    l->length++;
}

uint64_t pop(Obj *obj) {
    list_t *l = obj->list;
    if (l->length == 0) {
        printf("list index out of range\n");
        exit(1);
    }
    l->length--;
    return l->data[l->length];
}

void listset(Obj *list, uint64_t index, int64_t val) {
    list_t *l = list->list;
    if (index >= l->length) {
        printf("list index out of range\n");
        exit(1);
    }
    l->data[index] = val;
}

int64_t listget(Obj *list, uint64_t index) {
    list_t *l = list->list;
    if (index >= l->length) {
        printf("list index out of range\n");
        exit(1);
    }
    return l->data[index];
}

void list_set_all(Obj *list, void *val) {
    list_t *l = list->list;
    for (int i = 0; i < l->length; i++) {
        memcpy(l->data + i * sizeof(uint64_t), val, sizeof(uint64_t));
    }
}

void list_free(list_t *list) {
    GC_free(list->data);
    GC_free(list);
}

int len(Obj *list) { return list->list->length; }

Obj* concat(Obj *a, Obj *b) {
    str_t *astr = a->str;
    str_t *bstr = b->str;
    int64_t len = astr->len + bstr->len;
    char *ptr = (char *)GC_malloc(len + 1);
    memcpy(ptr, astr->ptr, astr->len);
    memcpy(ptr + astr->len, bstr->ptr, bstr->len);
    ptr[len] = '\0';
    str_t *str = (str_t *)GC_malloc(sizeof(str_t));
    str->len = len;
    str->ptr = ptr;
    str->constant = 0;
    Obj *obj = newobj(OBJ_STR);
    obj->str = str;
    return obj;
}

void chansend(Obj *chan, uint64_t val) {
    chan_t *c = chan->chan;
    pthread_mutex_lock(&c->m_mu);
    while (c->len == c->cap) {
        pthread_cond_wait(&c->w_cond, &c->m_mu);
    }
    chan_write(c, val);
    pthread_cond_signal(&c->r_cond);
    pthread_mutex_unlock(&c->m_mu);
}

uint64_t chanrecv(Obj *chan) {
    chan_t *c = chan->chan;
    pthread_mutex_lock(&c->m_mu);
    while (c->len == 0) {
        pthread_cond_wait(&c->r_cond, &c->m_mu);
    }
    uint64_t val;
    int res = chan_read(c, &val);
    pthread_cond_signal(&c->w_cond);
    pthread_mutex_unlock(&c->m_mu);
    if (res == CHAN_CLOSED) {
        printf("channel closed\n");
        exit(1);
    }
    return val;
}

int main() {
    GC_INIT();
    GC_expand_hp(1024 * 1024 * 1024);
    sahl_main();
    return 0;
}
