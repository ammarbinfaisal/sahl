#include <assert.h>
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

static inline void *malloc_or_die(size_t size) {
    void *ptr = malloc(size);
    if (ptr == NULL) {
        printf("malloc failed\n");
        exit(1);
    }
    return ptr;
}

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
    q = malloc_or_die(sizeof(LinkedList));

    if (q == NULL) {
        return q;
    }

    q->size = 0;
    q->head = NULL;
    q->tail = NULL;

    return q;
}

int enqueue(LinkedList *q, void *value) {
    Node *node = malloc_or_die(sizeof(Node));

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

    free(tmp);

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
            free(tmp->value);
        }

        free(tmp);
    }

    free(q);
}

// treadmil garbage collector

// Map

// algo taken from GFG

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

RBNode *new_rb_node(int64_t key) {
    RBNode *node = malloc_or_die(sizeof(RBNode));
    node->key = key;
    node->value = 0;
    node->color = RED;
    node->left = NULL;
    node->right = NULL;
    node->parent = NULL;
    return node;
}

// function to perform BST insertion of a node
RBNode *rb_insert(RBNode *trav, RBNode *temp) {
    // If the tree is empty,
    // return a new node
    if (trav == NULL) return temp;

    if (temp->key < trav->key) {
        trav->left = rb_insert(trav->left, temp);
        trav->left->parent = trav;
    } else {
        trav->right = rb_insert(trav->right, temp);
        trav->right->parent = trav;
    }

    // Return the (unchanged) node pointer
    return trav;
}

// Function performing right rotation
// of the passed node
void rb_rightrotate(RBNode **root, RBNode *temp) {
    RBNode *left = temp->left;
    temp->left = left->right;
    if (temp->left) temp->left->parent = temp;
    left->parent = temp->parent;
    if (!temp->parent)
        *root = left;
    else if (temp == temp->parent->left)
        temp->parent->left = left;
    else
        temp->parent->right = left;
    left->right = temp;
    temp->parent = left;
}

// Function performing left rotation
// of the passed node
void rb_leftrotate(RBNode **root, RBNode *temp) {
    RBNode *right = temp->right;
    temp->right = right->left;
    if (temp->right) temp->right->parent = temp;
    right->parent = temp->parent;
    if (!temp->parent)
        *root = right;
    else if (temp == temp->parent->left)
        temp->parent->left = right;
    else
        temp->parent->right = right;
    right->left = temp;
    temp->parent = right;
}

// This function fixes violations
// caused by rb_insert insertion
void rb_fixup(RBNode *root, RBNode *pt) {
    RBNode *parent_pt = NULL;
    RBNode *grand_parent_pt = NULL;

    while ((pt != root) && (pt->color != 0) && (pt->parent->color == 1)) {
        parent_pt = pt->parent;
        grand_parent_pt = pt->parent->parent;

        /*  Case : A
             Parent of pt is left child
             of Grand-parent of
           pt */
        if (parent_pt == grand_parent_pt->left) {

            RBNode *uncle_pt = grand_parent_pt->right;

            /* Case : 1
                The uncle of pt is also red
                Only Recoloring required */
            if (uncle_pt != NULL && uncle_pt->color == 1) {
                grand_parent_pt->color = 1;
                parent_pt->color = 0;
                uncle_pt->color = 0;
                pt = grand_parent_pt;
            }

            else {

                /* Case : 2
                     pt is right child of its parent
                     Left-rotation required */
                if (pt == parent_pt->right) {
                    rb_leftrotate(&root, parent_pt);
                    pt = parent_pt;
                    parent_pt = pt->parent;
                }

                /* Case : 3
                     pt is left child of its parent
                     Right-rotation required */
                rb_rightrotate(&root, grand_parent_pt);
                int t = parent_pt->color;
                parent_pt->color = grand_parent_pt->color;
                grand_parent_pt->color = t;
                pt = parent_pt;
            }
        }

        /* Case : B
             Parent of pt is right
             child of Grand-parent of
           pt */
        else {
            RBNode *uncle_pt = grand_parent_pt->left;

            /*  Case : 1
                The uncle of pt is also red
                Only Recoloring required */
            if ((uncle_pt != NULL) && (uncle_pt->color == 1)) {
                grand_parent_pt->color = 1;
                parent_pt->color = 0;
                uncle_pt->color = 0;
                pt = grand_parent_pt;
            } else {
                /* Case : 2
                   pt is left child of its parent
                   Right-rotation required */
                if (pt == parent_pt->left) {
                    rb_rightrotate(&root, parent_pt);
                    pt = parent_pt;
                    parent_pt = pt->parent;
                }

                /* Case : 3
                     pt is right child of its parent
                     Left-rotation required */
                rb_leftrotate(&root, grand_parent_pt);
                int t = parent_pt->color;
                parent_pt->color = grand_parent_pt->color;
                grand_parent_pt->color = t;
                pt = parent_pt;
            }
        }
    }
}

RBNode *rb_search(RBNode *root, uint64_t key) {
    if (root == NULL || key == root->key) return root;
    if (key < root->key) return rb_search(root->left, key);
    return rb_search(root->right, key);
}

LinkedList *rb_to_ll(RBNode *root) {
    LinkedList *ll = new_list();
    if (root == NULL) {
        return ll;
    }
    LinkedList *left = rb_to_ll(root->left);
    LinkedList *right = rb_to_ll(root->right);
    ll->head = left->head;
    ll->tail = left->tail;
    ll->size = left->size;
    if (root->color == RED) {
        enqueue(ll, (void *)root->key);
        enqueue(ll, (void *)root->value);
    }
    ll->tail->next = right->head;
    ll->tail = right->tail;
    ll->size += right->size;
    return ll;
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
    chan_t *c = malloc_or_die(sizeof(chan_t));
    c->q = new_list();
    c->len = 0;
    c->cap = capacity;

    int rc;

    rc = pthread_mutex_init(&c->m_mu, NULL);
    assert(rc == 0);
    rc = pthread_cond_init(&c->r_cond, NULL);
    assert(rc == 0);
    rc = pthread_cond_init(&c->w_cond, NULL);
    assert(rc == 0);
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

enum ObjType { OBJ_STR, OBJ_LIST, OBJ_CHAN, OBJ_MAP };

typedef enum ObjType ObjType;

enum GCColor { GC_GREY, GC_BLACK, GC_ECRU, GC_WHITE };

typedef enum GCColor GCColor;

struct Obj {
    ObjType type;
    GCColor color;
    int boxed_items;
    struct Obj *next;
    struct Obj *prev;
    union {
        str_t *str;
        list_t *list;
        chan_t *chan;
        RBNode *map;
    };
};

typedef struct Obj Obj;

static Obj *scan, *freed, *bottom, *top;
pthread_mutex_t grey_mu = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t grey_cond = PTHREAD_COND_INITIALIZER;

void str_free(str_t *str) {
    free(str->ptr);
    free(str);
}

void list_free(list_t *list) {
    free(list->data);
    free(list);
}

void rb_free(RBNode *root) {
    if (root == NULL) {
        return;
    }
    rb_free(root->left);
    rb_free(root->right);
    // free(root->key); if key is boxed
    free(root);
}

void chan_free(chan_t *chan) {
    free_linkedlist(chan->q);
    free(chan);
}

void unlink(Obj *obj) {
    if (obj->next != NULL) {
        obj->next->prev = obj->prev;
    }
    if (obj->prev != NULL) {
        obj->prev->next = obj->next;
    }
}

void linktop(Obj *obj) {
    printf("linktop(%p) -> %p\n", obj, top);
    // Attach to the front of top
    obj->next = top->next;
    obj->prev = /* circular */ top->prev;
    top->prev = obj; // Update the previous front's prev pointer
    top = obj;       // Update top to point to the new front
    printf("linktop(%p) -> %p\n", obj, top);
}

void darken(Obj *obj) {
    // printf("top = %p, bottom = %p, scan = %p\n", top, bottom, scan);
    printf("darkening %p\n", obj);
    assert(obj->color == GC_ECRU);
    unlink(obj);
    linktop(obj);
}

void free_obj(Obj *);

void advance() {
    // darken all pointers from scan
    if (scan->boxed_items) {
        switch (scan->type) {
        case OBJ_STR: {
            break;
        }
        case OBJ_LIST: {
            for (int i = 0; i < scan->list->length; i++) {
                int64_t val = scan->list->data[i];
                Obj *valobj = (Obj *)val;
                if (valobj->color == GC_ECRU) {
                    darken(valobj);
                }
            }
            break;
        }
        case OBJ_MAP: {
            LinkedList *ll = rb_to_ll(scan->map);
            LinkedList *curr = ll;
            while (ll->size) {
                dequeue(ll); // key
                int64_t val = (int64_t)dequeue(ll);
                Obj *valobj = (Obj *)val;
                if (valobj->color == GC_ECRU) {
                    darken(valobj);
                }
            }
            break;
        }
        case OBJ_CHAN: {
            LinkedList *ll = scan->chan->q;
            while (ll->size) {
                int64_t val = (int64_t)dequeue(ll);
                Obj *valobj = (Obj *)val;
                if (valobj->color == GC_ECRU) {
                    darken(valobj);
                }
            }
        }
        }
    }
    printf("scan = %p \t top = %p \t bottom = %p freed = %p\n", scan, top,
           bottom, freed);
    scan = scan->prev;
}

void free_obj(Obj *to_free) {
    printf("freeing %p\n", to_free);
    switch (to_free->type) {
    case OBJ_STR: {
        str_free(to_free->str);
        break;
    }
    case OBJ_LIST: {
        list_free(to_free->list);
        break;
    }
    case OBJ_CHAN: {
        chan_free(to_free->chan);
        break;
    }
    case OBJ_MAP: {
        rb_free(to_free->map);
        break;
    }
    }
}

void gc_collect() {
    if (freed == bottom) {
        printf("gc_collect:\n");
        printf("colececece scan = %p \t top = %p \t bottom = %p freed = %p\n",
               scan, top, bottom, freed);
        for (Obj *obj = scan; obj != freed; obj = obj->next) {
            printf("%p\n", obj);
            free_obj(obj);
            obj->color = GC_ECRU;
        }
        Obj *tmp = top;
        top = bottom;
        bottom = tmp;
    }
}

struct variant_t {
    uint64_t tag;
    uint64_t val;
};

typedef struct variant_t variant_t;

void iprint(int64_t i) { printf("%ld", i); }

void fprint(double f) { printf("%lf", f); }

void cprint(char c) { printf("%c", c); }

void bprint(int b) { printf("%s", b ? "true" : "false"); }

void exit_with(int32_t code) { exit(code); }

variant_t *make_variant(uint64_t val, uint64_t tag) {
    variant_t *v = (variant_t *)malloc_or_die(sizeof(variant_t));
    v->tag = tag;
    v->val = val;
    return v;
}

int is_variant(variant_t *v, uint64_t tag) { return v->tag == tag; }

int64_t get_variant(variant_t *v) { return v->val; }

Obj *newobj(ObjType ty, int boxed) {
    Obj *obj;
    // put between top and scan;
    if (boxed) {
        obj = top;
        top = top->prev;
    } else {
        obj = freed;
        freed = freed->next;
    }
    obj->type = ty;
    obj->boxed_items = boxed;
    advance();
    gc_collect();
    return obj;
}

Obj *make_string(char *ptr, int len) {
    str_t *str = (str_t *)malloc_or_die(sizeof(str_t));
    str->len = len;
    str->ptr = ptr;
    Obj *obj = newobj(OBJ_STR, 0);
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
    char *ptr = (char *)malloc_or_die(len + 1);
    memcpy(ptr, astr->ptr, astr->len);
    memcpy(ptr + astr->len, bstr->ptr, bstr->len);
    ptr[len] = '\0';
    str_t *str = (str_t *)malloc_or_die(sizeof(str_t));
    str->len = len;
    str->ptr = ptr;
    str->constant = 0;
    Obj *obj = newobj(OBJ_STR, 0);
    obj->str = str;
    return obj;
}

char strget(Obj *str, uint64_t index) {
    str_t *s = str->str;
    if (index >= s->len) {
        printf("string index out of range\n");
        exit(1);
    }
    return s->ptr[index];
}

int64_t mapget(Obj *map, int64_t key) {
    RBNode *node = rb_search(map->map, key);
    if (node == NULL) {
        printf("key not found\n");
        exit(1);
    }
    darken(map);
    return node->value;
}

void mapset(Obj *map, int64_t key, int64_t val) {
    RBNode *node = rb_search(map->map, key);
    if (node == NULL) {
        node = new_rb_node(key);
        map->map = rb_insert(map->map, node);
    }
    node->value = val;
}

Obj *make_list(size_t size, int boxed) {
    list_t *list = (list_t *)malloc_or_die(sizeof(list_t));
    list->cap = size;
    list->length = size;
    size_t newsize = size * sizeof(uint64_t);
    list->data = (uint64_t *)malloc_or_die(newsize);
    Obj *obj = newobj(OBJ_LIST, boxed);
    obj->list = list;
    return obj;
}

Obj *make_map(size_t size, int boxed) {
    Obj *obj = newobj(OBJ_MAP, boxed);
    obj->map = new_rb_node(0);
    return obj;
}

Obj *make_chan(size_t size, int boxed) {
    chan_t *chan = new_chan(size);
    Obj *obj = newobj(OBJ_CHAN, boxed);
    obj->chan = chan;
    return obj;
}

typedef Obj *(*MakeFn)(size_t len, int boxed);

static MakeFn make_fns[] = {make_list, make_map, make_chan};

Obj *make(int ty, size_t size) {
    int boxed = ty >> 8;
    ty = ty & 0xff - 5;
    Obj *obj = make_fns[ty](size, boxed);
    return obj;
}

void append(Obj *obj, int64_t val) {
    list_t *l = obj->list;
    if (l->length + 1 > l->cap) {
        l->cap = l->cap == 0 ? 8 : l->cap * 2;
        size_t newsize = l->cap * sizeof(uint64_t);

        l->data = (uint64_t *)realloc(l->data, newsize);
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
    if (list->color == GC_ECRU) {
        darken(list);
    }
    return l->data[index];
}

void list_set_all(Obj *list, void *val) {
    list_t *l = list->list;
    for (int i = 0; i < l->length; i++) {
        memcpy(l->data + i * sizeof(uint64_t), val, sizeof(uint64_t));
    }
}

int len(Obj *list) {
    if (list->type == OBJ_STR) {
        return list->str->len;
    }
    return list->list->length;
}

Obj *concat(Obj *a, Obj *b) {
    str_t *astr = a->str;
    str_t *bstr = b->str;
    int64_t len = astr->len + bstr->len;
    char *ptr = (char *)malloc_or_die(len + 1);
    memcpy(ptr, astr->ptr, astr->len);
    memcpy(ptr + astr->len, bstr->ptr, bstr->len);
    ptr[len] = '\0';
    str_t *str = (str_t *)malloc_or_die(sizeof(str_t));
    str->len = len;
    str->ptr = ptr;
    str->constant = 0;
    Obj *obj = newobj(OBJ_STR, 0);
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

void alloc_heap() {
    // allocate a circular doubly linked list of objects
    int len = 1000;
    Obj *objs = (Obj *)malloc_or_die(len * sizeof(Obj));
    Obj *prev = NULL;
    for (int i = 0; i < len; i++) {
        Obj *obj = objs + i;
        obj->color = GC_WHITE;
        obj->boxed_items = 0;
        obj->next = NULL;
        obj->prev = prev;
        if (prev != NULL) {
            prev->next = obj;
        }
        prev = obj;
    }
    // make it circular
    objs[0].prev = objs + len - 1;
    objs[len - 1].next = objs;
    top = objs;
    bottom = objs;
    scan = objs;
    freed = objs;
}

int main() {
    alloc_heap();
    sahl_main();
    return 0;
}
