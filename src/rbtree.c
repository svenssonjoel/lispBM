
#include "rbtree.h" 
#include "stdint.h"

cons_t *rot_r(cons_t *h) {

  cons_t *x;

  x = h->cdr.cons->car.cons;
  h->cdr.cons->car.cons = x->cdr.cons->cdr.cons;
  x->cdr.cons->cdr.cons = h;

  return x; 
}

cons_t *rot_l(cons_t *h) {

  cons_t *x;

  x = h->cdr.cons->cdr.cons;
  h->cdr.cons->cdr.cons = x->cdr.cons->car.cons;
  x->cdr.cons->car.cons = h;

  return x; 
}

int rbtree_is_empty(cons_t *tree) {

  if (GET_CAR_TYPE(tree->type) == NIL)
    return 1;
  else return 0;
  
}

cons_t *rbtree_create(void) {

  cons_t *root;
  uint32_t type = 0;
  
  root = heap_allocate_cell();

  if (!root) return NULL;

  root->car.cons = NULL;
  root->cdr.cons = heap_allocate_cell();

  /* set type of root cell */ 
  type = SET_CAR_TYPE(type,NIL);
  type = SET_CDR_TYPE(type,POINTER);
  type = SET_AUX_BIT(type,COLOR_BLACK,COLOR_BIT); 
  root->type = type;

  /* set type of cdr cell */ 

  type = 0;
  type = SET_CAR_TYPE(type,NIL);
  type = SET_CDR_TYPE(type,NIL);
  root->cdr.cons->type = type; 
  
  return root; 
}
