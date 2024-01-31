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
    enqueue(chan->q, (void*)v);
    chan->len++;
    return CHAN_OK;
}

static int chan_read(Chan *chan, Value *v) {
    if (chan->closed) {
        return CHAN_CLOSED;
    }
    *v = (Value)dequeue(chan->q);
    return CHAN_OK;
}

#endif
