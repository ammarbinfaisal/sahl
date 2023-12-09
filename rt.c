#include "rt.h"
#include <assert.h>
#include <stdlib.h>

// #define DEBUG

#ifdef DEBUG
#define DEBUG_printf(...) printf(__VA_ARGS__)
#else
#define DEBUG_printf(...)
#endif

void sahl_main();

static inline void *checked_malloc(size_t size) {
    void *ptr;
    ptr = GC_malloc(size);
    if (ptr == NULL) {
        printf("out of memory\n");
        exit(1);
    }
    return ptr;
}

// List

LinkedList *new_list() {
    LinkedList *q;
    q = checked_malloc(sizeof(LinkedList));

    if (q == NULL) {
        return q;
    }

    q->size = 0;
    q->head = NULL;
    q->tail = NULL;

    return q;
}

int enqueue(LinkedList *q, void *value) {
    Node *node = checked_malloc(sizeof(Node));

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
    Node *tmp = NULL;

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

// Map

// algo taken from GFG

RBNode *new_rb_node(int64_t key) {
    RBNode *node = checked_malloc(sizeof(RBNode));
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

// Chan

chan_t *new_chan(int capacity) {
    chan_t *c = malloc(sizeof(chan_t));
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

void iprint(int64_t i) { printf("%ld", i); }

void fprint(double f) { printf("%lf", f); }

void cprint(char c) { printf("%c", c); }

void bprint(int b) { printf("%s", b ? "true" : "false"); }

void exit_with(int32_t code) { exit(code); }

variant_t *make_variant(uint64_t val, uint64_t tag) {
    variant_t *v = (variant_t *)checked_malloc(sizeof(variant_t));
    v->tag = tag;
    v->val = val;
    return v;
}

int is_variant(variant_t *v, uint64_t tag) { return v->tag == tag; }

int64_t get_variant(variant_t *v) { return v->val; }

Obj *newobj(ObjType ty) {
    Obj *obj = (Obj *)checked_malloc(sizeof(Obj));
    obj->marked = 0;
    obj->type = ty;
    return obj;
}

Obj *make_string(char *ptr, int len) {
    str_t *str = (str_t *)checked_malloc(sizeof(str_t));
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
    char *ptr = (char *)checked_malloc(len + 1);
    memcpy(ptr, astr->ptr, astr->len);
    memcpy(ptr + astr->len, bstr->ptr, bstr->len);
    ptr[len] = '\0';
    str_t *str = (str_t *)checked_malloc(sizeof(str_t));
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

Obj *make_list(size_t size) {
    list_t *list = (list_t *)checked_malloc(sizeof(list_t));
    list->cap = size;
    list->length = size;
    size_t newsize = size * sizeof(uint64_t);
    list->data = (uint64_t *)checked_malloc(newsize);
    Obj *obj = newobj(OBJ_LIST);
    obj->list = list;
    return obj;
}

Obj *make_map() {
    Obj *obj = newobj(OBJ_MAP);
    obj->map = new_rb_node(0);
    return obj;
}

Obj *make_chan(size_t size) {
    chan_t *chan = new_chan(size);
    Obj *obj = newobj(OBJ_CHAN);
    obj->chan = chan;
    return obj;
}

typedef Obj *(*MakeFn)(size_t len);

static MakeFn make_fns[] = {make_list, make_map, make_chan};

Obj *make(int ty, size_t size) { return make_fns[ty - 5](size); }

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
    char *ptr = (char *)checked_malloc(len + 1);
    memcpy(ptr, astr->ptr, astr->len);
    memcpy(ptr + astr->len, bstr->ptr, bstr->len);
    ptr[len] = '\0';
    str_t *str = (str_t *)checked_malloc(sizeof(str_t));
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

void spawn(void *fn, void *arg) {
    pthread_t thread;
    int rc = pthread_create(&thread, NULL, fn, arg);
    assert(rc == 0);
}

int main() {
    GC_INIT();
    GC_expand_hp(1024 * 1024 * 256);
    sahl_main();
    return 0;
}
