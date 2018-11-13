
#include <stdio.h>

#include "print.h"
#include "heap.h"
#include "symrepr.h"


int simple_print(uint32_t t){

  char *str_ptr;
  
  if (IS_PTR(t)) {
    // TODO: Switch on the type of object pointed to.
    if ((t & PTR_TYPE_MASK) == PTR_TYPE_CONS) {
      printf("(");
      simple_print(read_car(ref_cell(t)));
      printf(" "); 
      simple_print(read_cdr(ref_cell(t)));
      printf(")");			
    }
    return 1; 
  } else { // Value, symbol 

    switch (t & VAL_TYPE_MASK) {
    case VAL_TYPE_SYMBOL:
      str_ptr = symrepr_lookup_name(DEC_SYM(t));
      printf("%s", str_ptr);
      break;
    case VAL_TYPE_I28:
      printf("%d", DEC_I28(t)); 
      break;
    case VAL_TYPE_U28:
      printf("%u", DEC_U28(t));
      break;
    case VAL_TYPE_CHAR: // dont yet have syntax for Char and String
      printf("#\\%c", DEC_CHAR(t));
      break;
    default:
      printf("simple_print: Error\n");
      return 0; 
    }
  }
  return 1;
}


/*
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
*/
