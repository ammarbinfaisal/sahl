#include "func.h"

CallFrame *new_call_frame(Func *func, CallFrame *prev) {
    CallFrame *frame = malloc(sizeof(CallFrame));
    frame->ip = 0;
    frame->func = func;
    frame->prev = prev;
    frame->depth = prev ? prev->depth + 1 : 0;
    frame->next = NULL;
    prev ? prev->next = frame : 0;
    return frame;
}

void free_call_frame(CallFrame *frame) {
    free(frame->locals);
    free(frame);
}
