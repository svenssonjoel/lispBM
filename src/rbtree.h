
#ifndef RBTREE_H_
#define RBTREE_H_

#include "heap0.h"

/* refers to the first AUX bit inte cons_t Type field. */ 
#define COLOR_BIT 0

#define COLOR_BLACK 0
#define COLOR_RED   1 

cons_t *rbtree_create(void);
int rbtree_is_empty(cons_t *tree); 

#endif 
