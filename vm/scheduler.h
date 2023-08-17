#ifndef SCHEDULER_H

#define SCHEDULER_H 1

#include "common.h"
#include "list.h"
#include <pthread.h>

struct Scheduler {
    int coro_running;
    LinkedList *coro_queue;

    // signalling when a thread is free
    pthread_cond_t vmq_cond;
    pthread_mutex_t vmq_mu;
};

typedef struct Scheduler Scheduler;

#endif