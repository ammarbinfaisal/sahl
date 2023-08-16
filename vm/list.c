#include "list.h"
#include <stdlib.h>

LinkedList *new_list(int capacity) {
    LinkedList *q;
    q = malloc(sizeof(LinkedList));

    if (q == NULL) {
        return q;
    }

    q->size = 0;
    q->max_size = capacity;
    q->head = NULL;
    q->tail = NULL;

    return q;
}

int enqueue(LinkedList *q, void *value) {
    if ((q->size + 1) > q->max_size) {
        return q->size;
    }

    Node *node = malloc(sizeof(Node));

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
