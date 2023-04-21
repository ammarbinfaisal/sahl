#include "conc.h"

Queue *new_queue(int capacity) {
    Queue *q = malloc(sizeof(Queue));
    q->capacity = capacity;
    q->length = 0;
    q->items = malloc(sizeof(Value) * capacity);
    return q;
}

Chan *new_chan(int capacity) {
    Chan *c = malloc(sizeof(Chan));
    c->q = new_queue(capacity);

    pthread_mutex_init(&c->m_mu, NULL);
    pthread_cond_init(&c->r_cond, NULL);
    pthread_cond_init(&c->w_cond, NULL);
    c->closed = 0;
    c->r_waiting = 0;
    c->w_waiting = 0;

    return c;
}
