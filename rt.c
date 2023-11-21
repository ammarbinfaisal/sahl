#include <gc/gc.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#define GROW_CAPACITY(cap) ((cap) < 8 ? 8 : (cap)*2)

// #define DEBUG

#ifdef DEBUG
#define DEBUG_printf(...) printf(__VA_ARGS__)
#else
#define DEBUG_printf(...)
#endif

void sahl_main();

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

enum ObjType { OBJ_STR, OBJ_LIST };

typedef enum ObjType ObjType;

struct Obj {
    ObjType type;
    int marked;
    union {
        str_t *str;
        list_t *list;
    };
};

typedef struct Obj Obj;

void iprint(int64_t i) { printf("%ld", i); }

void fprint(double f) { printf("%lf", f); }

void cprint(char c) { printf("%c", c); }

void bprint(int b) { printf("%s", b ? "true" : "false"); }

void exit_with(int32_t code) { exit(code); }

Obj *newobj(ObjType ty) {
    DEBUG_printf("malloc %ld\n", sizeof(Obj));
    Obj *obj = (Obj *)GC_malloc(sizeof(Obj));
    obj->marked = 0;
    obj->type = ty;
    return obj;
}

Obj *make_string(char *ptr, int len) {
    DEBUG_printf("malloc %ld\n", sizeof(str_t));
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
    DEBUG_printf("malloc %ld\n", len + 1);
    char *ptr = (char *)GC_malloc(len + 1);
    memcpy(ptr, astr->ptr, astr->len);
    memcpy(ptr + astr->len, bstr->ptr, bstr->len);
    ptr[len] = '\0';
    DEBUG_printf("malloc %ld\n", sizeof(str_t));
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
    DEBUG_printf("malloc %ld\n", sizeof(list_t));
    list_t *list = (list_t *)GC_malloc(sizeof(list_t));
    list->cap = size;
    list->length = size;
    DEBUG_printf("list size %ld\n", size);
    size_t newsize = size * sizeof(uint64_t);
    DEBUG_printf("malloc list els %ld\n", newsize);
    list->data = (uint64_t *)GC_malloc(newsize);
    Obj *obj = newobj(OBJ_LIST);
    obj->list = list;
    return obj;
}

typedef Obj *(*MakeFn)(size_t len);

static MakeFn make_fns[] = {make_list};

Obj *make(int ty, size_t size) {
    // the first 5 types are primitives
    // elemsize is set to 8 right now but it should be set to the size of the
    // primitive
    return make_fns[ty - 5](size);
}

void append(Obj *obj, int64_t val) {
    list_t *l = obj->list;
    if (l->length + 1 > l->cap) {
        l->cap = l->cap == 0 ? 8 : l->cap * 2;
        size_t newsize = l->cap * sizeof(uint64_t);
        DEBUG_printf("realloc %ld\n", newsize);
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

int main() {
    GC_INIT();
    sahl_main();
    return 0;
}
