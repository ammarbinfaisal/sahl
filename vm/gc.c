#include "gc.h"
#include <complex.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    void *ptr;
    int size;
    bool obj;
} worklist_items_t;

void *checked_malloc(size_t size) {
    void *ptr = malloc(size);
    if (ptr == NULL) {
        printf("out of memory\n");
        exit(1);
    }
    return ptr;
}
