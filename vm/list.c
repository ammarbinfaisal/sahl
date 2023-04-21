#include <stdlib.h>

#include "list.h"

LinkedList *new_list() {
    LinkedList *l = malloc(sizeof(LinkedList));
    l->next = NULL;
    return l;
}

void list_append(LinkedList **list, void *data) {
    LinkedList *tmp = *list;
    while (tmp->next != NULL) {
        tmp = tmp->next;
    }
    tmp->next = malloc(sizeof(LinkedList));
    tmp->next->data = data;
    tmp->next->next = NULL;
}

void list_prepend(LinkedList **list, void *data) {
    LinkedList *tmp = malloc(sizeof(LinkedList));
    tmp->data = data;
    tmp->next = *list;
    *list = tmp;
}

void list_remove(LinkedList **list, void *data) {
    LinkedList *tmp = *list;
    if (tmp->data == data) {
        *list = tmp->next;
        free(tmp);
        return;
    }
    while (tmp->next != NULL) {
        if (tmp->next->data == data) {
            LinkedList *tmp2 = tmp->next;
            tmp->next = tmp->next->next;
            free(tmp2);
            return;
        }
        tmp = tmp->next;
    }
}

void list_free(LinkedList *list) {
    LinkedList *tmp = list;
    while (tmp != NULL) {
        LinkedList *tmp2 = tmp;
        tmp = tmp->next;
        free(tmp2);
    }
}
