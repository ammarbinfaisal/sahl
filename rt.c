#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#define GROW_CAPACITY(cap) ((cap) < 8 ? 8 : (cap)*2)

// #define DEBUG

struct str_t {
    int64_t len;
    int constant;
    char *ptr;
};

typedef struct str_t str_t;

struct list_t {
    int cap;
    int length;
    int elemsize;
    void *data;
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

void iprint(int64_t i) {
    printf("%ld", i);
}

void fprint(double f) {
    printf("%lf", f);
}

void cprint(char c) {
    printf("%c", c);
}

void bprint(int b) {
    printf("%s", b ? "true" : "false");
}

void exit_with(int32_t code) { exit(code); }

Obj *newobj(ObjType ty) {
    Obj *obj = (Obj *)malloc(sizeof(Obj));
    obj->marked = 0;
    obj->type = ty;
    return obj;
}

Obj *make_string(char *ptr, int len) {
    str_t *str = (str_t *)malloc(sizeof(str_t));
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

void sprint(Obj *o) {
    printf("%s", o->str->ptr);
}

Obj *strcatt(Obj *aobj, Obj *bobj) {
#ifdef DEBUG
    printf("strcatt(%p, %p)\n", aobj, bobj);
#endif
    str_t *astr = aobj->str;
    str_t *bstr = aobj->str;
    int64_t len = astr->len + bstr->len;
    char *ptr = (char *)malloc(len + 1);
    memcpy(ptr, astr->ptr, astr->len);
    memcpy(ptr + astr->len, bstr->ptr, bstr->len);
    ptr[len] = '\0';
    str_t *str = (str_t *)malloc(sizeof(str_t));
    str->len = len;
    str->ptr = ptr;
    str->constant = 0;
    Obj *obj = newobj(OBJ_STR);
    obj->str = str;
    return obj;
}

void str_free(str_t *str) {
    if (!str->constant) {
        free(str->ptr);
    }
    free(str);
}

Obj *make_list(int size, int elemsize) {
    list_t *list = (list_t *)malloc(sizeof(list_t));
    list->cap = size;
    list->length = size;
    list->elemsize = elemsize;
    list->data = (uint64_t *)malloc(elemsize * size);
    Obj *obj = newobj(OBJ_LIST);
    obj->list = list;
    return obj;
}

typedef Obj* (*MakeFn)(int len, int elemsize);

static MakeFn make_fns[] = {make_list};

Obj* make(int ty, int size) {
    // the first 5 types are primitives
    // elemsize is set to 8 right now but it should be set to the size of the primitive
    return make_fns[ty-5](size, 8); 
}

void append(Obj *obj, int64_t val) {
    list_t *l = obj->list;
    if (l->length + 1 > l->cap) {
        l->cap = l->cap == 0 ? 8 : l->cap * 2;
        l->data = (uint64_t *)realloc(l->data, l->elemsize * l->cap);
    }
    switch (l->elemsize) {
    case 1:
        *(uint8_t *)(l->data + l->length * l->elemsize) = *(uint8_t *)&val;
        break;
    case 2:
        *(uint16_t *)(l->data + l->length * l->elemsize) = *(uint16_t *)&val;
        break;
    case 4:
        *(uint32_t *)(l->data + l->length * l->elemsize) = *(uint32_t *)&val;
        break;
    case 8:
        *(uint64_t *)(l->data + l->length * l->elemsize) = *(uint64_t *)&val;
        break;
    }
    l->length++;
}

void listset(Obj *list, uint64_t index, int64_t val) {
    list_t *l = list->list;
    if (index >= l->length) {
        printf("list index out of range\n");
        exit(1);
    }
    switch (l->elemsize) {
    case 1:
        *(uint8_t *)(l->data + index * l->elemsize) = *(uint8_t *)&val;
        break;
    case 2:
        *(uint16_t *)(l->data + index * l->elemsize) = *(uint16_t *)&val;
        break;
    case 4:
        *(uint32_t *)(l->data + index * l->elemsize) = *(uint32_t *)&val;
        break;
    case 8:
        *(uint64_t *)(l->data + index * l->elemsize) = *(uint64_t *)&val;
        break;
    }
}

int64_t listget(Obj *list, uint64_t index) {
    list_t *l = list->list;
    if (index >= l->length) {
        printf("list index out of range\n");
        exit(1);
    }
    switch (l->elemsize) {
    case 1:
        return *(uint8_t *)(l->data + index * l->elemsize);
    case 2:
        return *(uint16_t *)(l->data + index * l->elemsize);
    case 4:
        return *(uint32_t *)(l->data + index * l->elemsize);
    case 8:
        return *(uint64_t *)(l->data + index * l->elemsize);
    }
    return 0;
}

void list_set_all(Obj *list, void *val) {
    list_t *l = list->list;
    for (int i = 0; i < l->length; i++) {
        memcpy(l->data + i * l->elemsize, val, l->elemsize);
    }
}

void list_free(list_t *list) {
    free(list->data);
    free(list);
}

int len(Obj *list) { return list->list->length; }
