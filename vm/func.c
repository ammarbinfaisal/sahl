#include "func.h"
#include "gc.h"

CallFrame *new_call_frame(Func *func, CallFrame *prev) {
    CallFrame *frame = checked_malloc(sizeof(CallFrame));
    frame->ip = 0;
    frame->func = func;
    frame->prev = prev;
    frame->depth = prev ? prev->depth + 1 : 0;
    frame->next = NULL;
    frame->stackmap = NULL;
    prev ? prev->next = frame : 0;
    return frame;
}

void free_call_frame(CallFrame *frame) {
    // free stackmap
    if (frame->stackmap) {
        free(frame->stackmap->bits);
        free(frame->stackmap);
    }
        
    free(frame->locals);
    free(frame);
}
