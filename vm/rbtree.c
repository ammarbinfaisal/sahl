#include <stdlib.h>

#include "gc.h"
#include "rbtree.h"

// algo taken from GFG

RBNode *new_rb_node(Value key) {
    RBNode *node = checked_malloc(sizeof(RBNode));
    node->key = key;
    node->value = 0;
    node->color = RED;
    node->left = NULL;
    node->right = NULL;
    node->parent = NULL;
    return node;
}

// function to perform BST insertion of a node
RBNode *rb_insert(RBNode *trav, RBNode *temp) {
    // If the tree is empty,
    // return a new node
    if (trav == NULL) return temp;

    if (temp->key < trav->key) {
        trav->left = rb_insert(trav->left, temp);
        trav->left->parent = trav;
    } else {
        trav->right = rb_insert(trav->right, temp);
        trav->right->parent = trav;
    }

    // Return the (unchanged) node pointer
    return trav;
}

// Function performing right rotation
// of the passed node
void rb_rightrotate(RBNode **root, RBNode *temp) {
    RBNode *left = temp->left;
    temp->left = left->right;
    if (temp->left) temp->left->parent = temp;
    left->parent = temp->parent;
    if (!temp->parent)
        *root = left;
    else if (temp == temp->parent->left)
        temp->parent->left = left;
    else
        temp->parent->right = left;
    left->right = temp;
    temp->parent = left;
}

// Function performing left rotation
// of the passed node
void rb_leftrotate(RBNode **root, RBNode *temp) {
    RBNode *right = temp->right;
    temp->right = right->left;
    if (temp->right) temp->right->parent = temp;
    right->parent = temp->parent;
    if (!temp->parent)
        *root = right;
    else if (temp == temp->parent->left)
        temp->parent->left = right;
    else
        temp->parent->right = right;
    right->left = temp;
    temp->parent = right;
}

// This function fixes violations
// caused by rb_insert insertion
void rb_fixup(RBNode *root, RBNode *pt) {
    RBNode *parent_pt = NULL;
    RBNode *grand_parent_pt = NULL;

    while ((pt != root) && (pt->color != 0) && (pt->parent->color == 1)) {
        parent_pt = pt->parent;
        grand_parent_pt = pt->parent->parent;

        /*  Case : A
             Parent of pt is left child
             of Grand-parent of
           pt */
        if (parent_pt == grand_parent_pt->left) {

            RBNode *uncle_pt = grand_parent_pt->right;

            /* Case : 1
                The uncle of pt is also red
                Only Recoloring required */
            if (uncle_pt != NULL && uncle_pt->color == 1) {
                grand_parent_pt->color = 1;
                parent_pt->color = 0;
                uncle_pt->color = 0;
                pt = grand_parent_pt;
            }

            else {

                /* Case : 2
                     pt is right child of its parent
                     Left-rotation required */
                if (pt == parent_pt->right) {
                    rb_leftrotate(&root, parent_pt);
                    pt = parent_pt;
                    parent_pt = pt->parent;
                }

                /* Case : 3
                     pt is left child of its parent
                     Right-rotation required */
                rb_rightrotate(&root, grand_parent_pt);
                int t = parent_pt->color;
                parent_pt->color = grand_parent_pt->color;
                grand_parent_pt->color = t;
                pt = parent_pt;
            }
        }

        /* Case : B
             Parent of pt is right
             child of Grand-parent of
           pt */
        else {
            RBNode *uncle_pt = grand_parent_pt->left;

            /*  Case : 1
                The uncle of pt is also red
                Only Recoloring required */
            if ((uncle_pt != NULL) && (uncle_pt->color == 1)) {
                grand_parent_pt->color = 1;
                parent_pt->color = 0;
                uncle_pt->color = 0;
                pt = grand_parent_pt;
            } else {
                /* Case : 2
                   pt is left child of its parent
                   Right-rotation required */
                if (pt == parent_pt->left) {
                    rb_rightrotate(&root, parent_pt);
                    pt = parent_pt;
                    parent_pt = pt->parent;
                }

                /* Case : 3
                     pt is right child of its parent
                     Left-rotation required */
                rb_leftrotate(&root, grand_parent_pt);
                int t = parent_pt->color;
                parent_pt->color = grand_parent_pt->color;
                grand_parent_pt->color = t;
                pt = parent_pt;
            }
        }
    }
}

RBNode *rb_search(RBNode *root, Value key) {
    if (root == NULL || key == root->key) return root;
    if (key < root->key) return rb_search(root->left, key);
    return rb_search(root->right, key);
}

LinkedListRB *rb_to_ll(RBNode *root) {
    LinkedListRB *head = NULL;
    LinkedListRB *tail = NULL;
    if (root == NULL) {
        return NULL;
    }
    LinkedListRB *node = checked_malloc(sizeof(LinkedListRB));
    node->key = root->key;
    node->value = root->value;
    node->next = NULL;
    head = node;
    tail = node;
    LinkedListRB *left = rb_to_ll(root->left);
    LinkedListRB *right = rb_to_ll(root->right);
    if (left != NULL) {
        tail->next = left;
        while (tail->next != NULL) {
            tail = tail->next;
        }
    }
    if (right != NULL) {
        tail->next = right;
        while (tail->next != NULL) {
            tail = tail->next;
        }
    }
    return head;
}

void rb_free(RBNode *root) {
    if (root == NULL) {
        return;
    }
    rb_free(root->left);
    rb_free(root->right);
    free(root);
}
