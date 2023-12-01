#include "read.h"
#include "gc.h"
#include <stdlib.h>
#include <string.h>

char *read_string(uint8_t *code, int idx, int len) {
    char *str = checked_malloc(len + 1);
    memcpy(str, code + idx, len);
    str[len] = '\0';
    return str;
}
