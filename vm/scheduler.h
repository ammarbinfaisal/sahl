#include "coro.h"
#ifndef SCHEDULER_H

#define SCHEDULER_H 1

#include "common.h"
#include "list.h"
#include <pthread.h>

struct Scheduler {
    int coros_to_spawn;
    int coro_running;
    LinkedList *coro_queue;

    coro_context** main_ctxes;
    coro_context** coro_ctxes;

    // signalling when a thread is free
    pthread_cond_t vmq_cond;
    pthread_mutex_t vmq_mu;

    // coro running
    pthread_cond_t coro_cond;
    pthread_mutex_t coro_mu;

    // coro creation
    pthread_mutex_t coro_create_mu;
};

typedef struct Scheduler Scheduler;

struct CoroArg {
    coro_context *main_ctx;
    coro_context *coro_ctx;
    VM *vm;
};

typedef struct CoroArg CoroArg;

#endif