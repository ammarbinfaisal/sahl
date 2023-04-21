#include "func.h"

CallFrame *new_call_frame(Func *func, CallFrame *prev) {
    CallFrame *frame = malloc(sizeof(CallFrame));
    frame->ip = 0;
    frame->func = func;
    frame->locals_count = 0;
    frame->locals_capacity = 0;
    frame->locals = NULL;
    frame->prev = prev;
    frame->depth = prev ? prev->depth + 1 : 0;
    return frame;
}

void free_call_frame(CallFrame *frame) {
    free(frame->locals);
    free(frame);
}
