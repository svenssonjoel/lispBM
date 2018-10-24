
#include "eval0.h"

#include "symtab.h"
#include "heap0.h" 

#include <stdio.h>
/* TODO: Experiment with evaluation */


cons_t * eval(cons_t *cell) {
  
  switch (GET_CAR_TYPE(cell->type))
    {
    case NIL:  printf("() "); break;
    case INTEGER: printf(" %d ", cell->car.i); break;
    case FLOAT: printf(" %f ", cell->car.f); break;
    case SYMBOL: printf("SYMBOL"); break;
    case POINTER:
      printf("POINTER->");
      if (cell->car.cons == NULL) printf("NULL POINTER!\n");
      eval(cell->car.cons);
      break; 
    }
  return NULL;
}
  

