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

#define ALIGN_STACK asm("and $-16, %rsp");

void iprint(int64_t i) {
    ALIGN_STACK
    printf("%ld", i);
}

void fprint(double f) {
    ALIGN_STACK
    printf("%lf", f);
}

void cprint(char c) {
    ALIGN_STACK
    printf("%c", c);
}

void bprint(int b) {
    ALIGN_STACK
    printf("%s", b ? "true" : "false");
}

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

Obj *newobj(ObjType ty) {
    Obj *obj = (Obj *)malloc(sizeof(Obj));
    obj->marked = 0;
    obj->type = ty;
    return obj;
}

Obj *newstr(char *ptr) {
    str_t *str = (str_t *)malloc(sizeof(str_t));
    str->len = strlen(ptr);
    str->ptr = ptr;
    str->constant = 1;
    Obj *obj = newobj(OBJ_STR);
    obj->str = str;

#ifdef DEBUG
    ALIGN_STACK
    printf("newstr(%p) -> %p\n", ptr, obj);
#endif
    return obj;
}

void sprint(Obj *s) {
    str_t *str = s->str;
    ALIGN_STACK
    printf("%s", str->ptr);
}

Obj *strcatt(Obj *aobj, Obj *bobj) {
#ifdef DEBUG
    ALIGN_STACK
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

Obj *new_list(uint8_t elemsize, int len) {
    Obj *obj = newobj(OBJ_LIST);
    obj->list = (list_t *)malloc(sizeof(list_t));
    obj->list->length = len;
    obj->list->cap = len ? len : 8;
    obj->list->data = (uint64_t *)malloc(elemsize * obj->list->cap);
    obj->list->elemsize = elemsize;
    return obj;
}

void list_append(Obj *obj, void *val) {
    list_t *l = obj->list;
    if (l->length + 1 > l->cap) {
        l->cap = l->cap == 0 ? 8 : l->cap * 2;
        l->data = (uint64_t *)realloc(l->data, l->elemsize * l->cap);
    }
    memcpy(l->data + l->length * l->elemsize, val, l->elemsize);
    l->length++;
}

void list_set(Obj *list, uint64_t index, void *val) {
    list_t *l = list->list;
    if (index >= l->length) {
        ALIGN_STACK
        printf("list index out of range\n");
        exit(1);
    }
    switch (l->elemsize) {
    case 1:
        *(uint8_t *)(l->data + index * l->elemsize) = *(uint8_t *)val;
        break;
    case 2:
        *(uint16_t *)(l->data + index * l->elemsize) = *(uint16_t *)val;
        break;
    case 4:
        *(uint32_t *)(l->data + index * l->elemsize) = *(uint32_t *)val;
        break;
    case 8:
        *(uint64_t *)(l->data + index * l->elemsize) = *(uint64_t *)val;
        break;
    }
}

void list_get(Obj *list, uint64_t index, void *val) {
    list_t *l = list->list;
    if (index >= l->length) {
        ALIGN_STACK
        printf("list index out of range\n");
        exit(1);
    }
    switch (l->elemsize) {
    case 1:
        *(uint8_t *)val = *(uint8_t *)(l->data + index * l->elemsize);
        break;
    case 2:
        *(uint16_t *)val = *(uint16_t *)(l->data + index * l->elemsize);
        break;
    case 4:
        *(uint32_t *)val = *(uint32_t *)(l->data + index * l->elemsize);
        break;
    case 8:
        *(uint64_t *)val = *(uint64_t *)(l->data + index * l->elemsize);
        break;
    }
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

int list_len(Obj *list) { return list->list->length; }
