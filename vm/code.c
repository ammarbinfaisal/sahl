#include "code.h"
#include "gc.h"

Code *read_bytecode(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (file == NULL) {
        printf("Error: Could not open file %s", filename);
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *buffer = checked_malloc(size);
    fread(buffer, 1, size, file);
    fclose(file);
    Code *code = checked_malloc(sizeof(Code));
    code->bytes = buffer;
    code->length = size;
    return code;
}
