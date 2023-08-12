#ifndef LIST_H

#define LIST_H

struct LinkedList {
    struct LinkedList *next;
    struct LinkedList *prev;
    struct LinkedList *last;
    void *data;
};

typedef struct LinkedList LinkedList;

LinkedList *new_list();
void list_append(LinkedList *list, void *data);
void list_prepend(LinkedList **list, void *data);
void* list_pop(LinkedList *list);
void list_free(LinkedList *list);

#endif