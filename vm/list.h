#ifndef LIST_H

#define LIST_H


struct Node {
    void *value;
    struct Node *next;
};

typedef struct Node Node;

struct LinkedList {
    int size;
    int max_size;
    struct Node *head;
    struct Node *tail;
};

typedef struct LinkedList LinkedList;

LinkedList *new_list(int capacity);

int enqueue(LinkedList *q, void *value);

void *dequeue(LinkedList *q);

void free_linkedlist(LinkedList *q);

#endif