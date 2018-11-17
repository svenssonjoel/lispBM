/*
    Copyright 2018 Joel Svensson	svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

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
      simple_print(car(t));
      printf(" "); 
      simple_print(cdr(t));
      printf(")");			
    }
    return 1; 
  } else { // Value, symbol 

    switch (t & VAL_TYPE_MASK) {
    case VAL_TYPE_SYMBOL:
      str_ptr = symrepr_lookup_name(DEC_SYM(t));
      if (str_ptr == NULL) {
	printf("Error: Symbol not in table %u\n", DEC_SYM(t));
      } else {  
	printf("%s", str_ptr);
      }
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
