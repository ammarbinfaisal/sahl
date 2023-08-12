#include <stdlib.h>

#include "list.h"

LinkedList *new_list() {
    LinkedList *l = malloc(sizeof(LinkedList));
    l->next = NULL;
    l->prev = NULL;
    l->last = l;
    return l;
}

void list_append(LinkedList *list, void *data) {
    LinkedList *tmp = malloc(sizeof(LinkedList));
    tmp->next = NULL;
    tmp->prev = list->last;
    tmp->last = tmp;
    list->last->data = data;
    list->last->next = tmp;
    list->last = tmp;
}

void list_prepend(LinkedList **list, void *data) {
    LinkedList *tmp = malloc(sizeof(LinkedList));
    tmp->data = data;
    tmp->next = *list;
    tmp->prev = NULL;
    *list = tmp;
}

void* list_pop(LinkedList *list) {
    LinkedList *tmp = list->last;
    list->last = tmp->prev;
    list->last->next = NULL;
    void *data = tmp->data;
    free(tmp);
    return data;
}

void list_free(LinkedList *list) {
    LinkedList *tmp = list;
    while (tmp != NULL) {
        LinkedList *tmp2 = tmp;
        tmp = tmp->next;
        free(tmp2);
    }
}
