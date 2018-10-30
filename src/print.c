
#include "print.h"
#include "heap0.h"
#include "symtab.h"

int simple_print(cons_t *t) {

  cons_t *curr = t; 

  printf("("); 
  
  switch (GET_CAR_TYPE(curr->type)) {
  case NIL:
    printf (" () ");
    break;
  case INTEGER:
    printf(" %d ", curr->car.i); 
    break;
  case FLOAT:
    printf(" %f ", curr->car.f); 
    break;
    
  case SYMBOL:
    printf(" %d ", curr->car.s); 
      break;
      
  case POINTER:
    simple_print(curr->car.cons);
      break;
  }

  switch (GET_CDR_TYPE(curr->type)) {
  case NIL:
    printf("()");
    break;
  case INTEGER:
    printf(" %d ", curr->cdr.i); 
    break;
  case FLOAT:
    printf(" %f ", curr->cdr.f); 
    break;
    
  case SYMBOL:
    printf(" %d ", curr->cdr.s); 
      break;
      
  case POINTER:
    simple_print(curr->cdr.cons);
      break;
  }

  printf(")"); 

  return 1; // TODO: ERROR CHECKING 
}
