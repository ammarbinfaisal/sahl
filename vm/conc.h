#ifndef CONC_H

#define CONC_H

#include <pthread.h>
#include <stdlib.h>

#include "common.h"

enum ChanOpResult { CHAN_OK, CHAN_CLOSED, CHAN_FULL };

Queue *new_queue(int capacity);
RingBuffer *new_ring_buffer(int capacity);
void rbuf_write(RingBuffer *rb, Value v);
Value rbuf_read(RingBuffer *rb);
Chan *new_chan(int capacity);

static int chan_write(Chan *chan, Value v) {
    pthread_mutex_lock(&chan->m_mu);
    while (chan->q->length == chan->q->capacity) {
        chan->w_waiting++;
        pthread_cond_wait(&chan->w_cond, &chan->m_mu);
        chan->w_waiting--;
    }
    chan->q->items[chan->q->length++] = v;
    if (chan->r_waiting > 0) {
        pthread_cond_signal(&chan->r_cond);
    }
    pthread_mutex_unlock(&chan->m_mu);
    return CHAN_OK;
}

static int chan_read(Chan *chan, Value *v) {
    pthread_mutex_lock(&chan->m_mu);
    while (chan->q->length == 0) {
        if (chan->closed) {
            pthread_mutex_unlock(&chan->m_mu);
            return CHAN_CLOSED;
        }
        chan->r_waiting++;
        pthread_cond_wait(&chan->r_cond, &chan->m_mu);
        chan->r_waiting--;
    }
    *v = chan->q->items[--chan->q->length];
    if (chan->w_waiting > 0) {
        pthread_cond_signal(&chan->w_cond);
    }
    pthread_mutex_unlock(&chan->m_mu);
    return CHAN_OK;
}

#endif
