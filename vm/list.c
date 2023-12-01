#include "list.h"
#include "gc.h"
#include <stdlib.h>

LinkedList *new_list() {
    LinkedList *q;
    q = checked_malloc(sizeof(LinkedList));

    if (q == NULL) {
        return q;
    }

    q->size = 0;
    q->head = NULL;
    q->tail = NULL;

    return q;
}

int enqueue(LinkedList *q, void *value) {
    Node *node = checked_malloc(sizeof(Node));

    if (node == NULL) {
        return q->size;
    }

    node->value = value;
    node->next = NULL;

    if (q->head == NULL) {
        q->head = node;
        q->tail = node;
        q->size = 1;

        return q->size;
    }

    q->tail->next = node;
    q->tail = node;
    q->size += 1;

    return q->size;
}

void *dequeue(LinkedList *q) {
    if (q->size == 0) {
        return NULL;
    }

    void *value = NULL;
    struct Node *tmp = NULL;

    value = q->head->value;
    tmp = q->head;
    q->head = q->head->next;
    q->size -= 1;

    free(tmp);

    return value;
}

void free_linkedlist(LinkedList *q) {
    if (q == NULL) {
        return;
    }

    while (q->head != NULL) {
        Node *tmp = q->head;
        q->head = q->head->next;
        if (tmp->value != NULL) {
            free(tmp->value);
        }

        free(tmp);
    }

    free(q);
}
