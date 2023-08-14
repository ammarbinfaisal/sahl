#ifndef RB_TREE_H

#define RB_TREE_H

#define RED 0
#define BLACK 1

#include "common.h"

struct LinkedListRB {
    struct LinkedListRB *next;
    Value key;
    Value value;
};

typedef struct LinkedListRB LinkedListRB;

RBNode *new_rb_node(Value key);
RBNode *rb_insert(RBNode *root, RBNode *node);
void rb_rightrotate(RBNode **root, RBNode *temp);
void rb_leftrotate(RBNode **root, RBNode *temp);
void rb_fixup(RBNode *root, RBNode *pt);
RBNode *rb_search(RBNode *root, Value key);
LinkedListRB *rb_to_ll(RBNode *root);
void rb_free(RBNode *root);

#endif
