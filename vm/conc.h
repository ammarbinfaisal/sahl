#ifndef CONC_H

#define CONC_H

#include <pthread.h>
#include <stdlib.h>

#include "common.h"
#include "list.h"

enum ChanOpResult { CHAN_OK, CHAN_CLOSED, CHAN_FULL };

Queue *new_queue(int capacity);
RingBuffer *new_ring_buffer(int capacity);
void rbuf_write(RingBuffer *rb, Value v);
Value rbuf_read(RingBuffer *rb);
Chan *new_chan(int capacity);
void close_chan(Chan *c);

static int chan_write(Chan *chan, Value v) {
    if (chan->closed) {
        return CHAN_CLOSED;
    }
    pthread_mutex_lock(&chan->m_mu);
    enqueue(chan->q, (void*)v);
    chan->len++;
    if (chan->r_waiting > 0) {
        pthread_cond_signal(&chan->r_cond);
    }
    pthread_mutex_unlock(&chan->m_mu);
    return CHAN_OK;
}

static int chan_read(Chan *chan, Value *v) {
    if (chan->closed) {
        return CHAN_CLOSED;
    }
    pthread_mutex_lock(&chan->m_mu);
    while (chan->len == 0) {
        if (chan->closed) {
            pthread_mutex_unlock(&chan->m_mu);
            return CHAN_CLOSED;
        }
        chan->r_waiting++;
        pthread_cond_wait(&chan->r_cond, &chan->m_mu);
        chan->r_waiting--;
    }
    *v = (Value)dequeue(chan->q);
    pthread_mutex_unlock(&chan->m_mu);
    return CHAN_OK;
}

#endif
