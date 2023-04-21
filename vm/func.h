#ifndef FUNC_H

#define FUNC_H

#include "common.h"
#include <stdlib.h>

CallFrame *new_call_frame(Func *func, CallFrame *prev);
void free_call_frame(CallFrame *frame);

#endif
