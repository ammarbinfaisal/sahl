#include "conc.h"
#include "gc.h"
#include "list.h"

Queue *new_queue(int capacity) {
    Queue *q = malloc(sizeof(Queue));
    q->capacity = capacity;
    q->length = 0;
    q->items = malloc(sizeof(Value) * capacity);
    return q;
}

Chan *new_chan(int capacity) {
    Chan *c = malloc(sizeof(Chan));
    c->q = new_list();
    c->len = 0;
    c->cap = capacity;

    pthread_mutex_init(&c->m_mu, NULL);
    pthread_cond_init(&c->r_cond, NULL);
    pthread_cond_init(&c->w_cond, NULL);
    c->closed = 0;
    c->r_waiting = 0;

    return c;
}

void close_chan(Chan *c) {
    pthread_mutex_lock(&c->m_mu);
    c->closed = 1;
    pthread_cond_broadcast(&c->r_cond);
    pthread_cond_broadcast(&c->w_cond);
    pthread_mutex_unlock(&c->m_mu);
}

void rbuf_write(RingBuffer *rb, Value v) {
    rb->items[rb->head] = v;
    rb->head = (rb->head + 1) % rb->capacity;
    rb->length++;
}

Value rbuf_read(RingBuffer *rb) {
    Value v = rb->items[rb->tail];
    rb->tail = (rb->tail + 1) % rb->capacity;
    rb->length--;
    return v;
}

RingBuffer *new_ring_buffer(int capacity) {
    RingBuffer *rb = malloc(sizeof(RingBuffer));
    rb->capacity = capacity;
    rb->length = 0;
    rb->head = 0;
    rb->tail = 0;
    rb->items = malloc(sizeof(Value) * capacity);
    return rb;
}
