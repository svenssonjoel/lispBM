
#include "eval0.h"

#include "symtab.h"
#include "heap0.h"
#include "built_in.h"

#include <stdio.h>
#include <stdint.h> 

/* TODO: Experiment with evaluation */
cons_t *eval_(cons_t *, int); 

cons_t *eval(cons_t *cell) {
  return eval_(cell, 1); 
}


/* Experiment with evaluation in relation to an abstract machine */ 
// TODO: Want to do the heap changes before tackling this part. 
typedef struct {
  uint32_t A;   // Maybe have some registers for values 
  uint32_t B; 
  uint32_t res; // result register: pointer to list on heap or a value (for example)
  uint32_t env; // Pointer to environment assoc-list on heap (could also be a tree) (Maybe wont need this)

  uint32_t flags; // NZCVQ etc

  uint32_t sp;  // thoughts. 
  uint32_t fp;

} AbstractMachine; 

// Future potential type of eval 
// int eval_AM(AbstractMachine *m, cons_t *program);


cons_t *eval_(cons_t *cell, int head_position) {

  /* The result of evaluation will be a reduced expression 
     represented by heap allocated cells */ 
  cons_t *result = heap_allocate_cell(); 
  uint32_t type = 0;
  int head_is_symbol = 0; 
  cons_t *ptr;

  /* if the cell we are evaluating is part of a list, 
     I think the result will be part of a list. 
     Unless the list represents a function application 
     in which case the result could be a single value 
     or even nothing at all ( I guess). */
  if (GET_CONS_TYPE(cell->type) == 1) {
    type = SET_CONS_TYPE(type,1); 
  }
    
  switch (GET_CAR_TYPE(cell->type))
    {
    case NIL:
      type = SET_CAR_TYPE(type, NIL);
      result->type = type;
      break; 
      
    case INTEGER:
      type = SET_CAR_TYPE(type,INTEGER);
      result->type = type;
      result->car.i = cell->car.i;
      break;
     
    case FLOAT:
      type = SET_CAR_TYPE(type,FLOAT);
      result->type = type;
      result->car.f = cell->car.f;
      break; 

    case SYMBOL:
      if (head_position) head_is_symbol = 1;
      
      type = SET_CAR_TYPE(type, SYMBOL);
      result->type = type;
      result->car.s = cell->car.s;
      break;
        
    case POINTER:
      if (cell->car.cons == NULL) return NULL; /* this would be bad */ 
      ptr = eval_(cell->car.cons, 1);

      if (GET_CONS_TYPE(ptr->type) == 1) { /* the result of evaluation is a list */
	type = SET_CAR_TYPE(type, POINTER);
	result->type = type;
	result->car.cons = ptr;
      } else {
	/* Here result should not contain a ptr in car position. 
	   But rather a copy what the eval result is. */
	type = GET_CAR_TYPE(ptr->type);
	result->type = SET_CAR_TYPE(result->type, type);
	switch(GET_CAR_TYPE(ptr->type)) {
	case INTEGER:
	  result->car.i = ptr->car.i;
	  break;
	case FLOAT:
	  result->car.f = ptr->car.f;
	  break;
	case SYMBOL:
	  result->car.s = ptr->car.s; 
	  break;
	case NIL:
	  result->car.i = 0; /* hack */ 
	  break; 
	}
      }
      break; 
    }

  
  switch(GET_CDR_TYPE(cell->type))
    {
    case NIL:
      type = SET_CDR_TYPE(type, NIL);
      result->type = type;
      break;
    case INTEGER:
      type = SET_CDR_TYPE(type, INTEGER);
      result->type = type;
      result->cdr.i = cell->cdr.i;
      break;

    case FLOAT:
      type = SET_CDR_TYPE(type,FLOAT);
      result->type = type;
      result->cdr.f = cell->cdr.f;
      break; 

    case SYMBOL: 
      type = SET_CDR_TYPE(type, SYMBOL);
      result->type = type;
      result->cdr.s = cell->cdr.s;
      break;
      
    case POINTER: /* move evaluation to next cell in a list */
      type = SET_CDR_TYPE(type,POINTER); 
      if (cell->cdr.cons == NULL) { printf( "NULL CASE!\n");  return NULL; } 
      ptr = eval_(cell->cdr.cons, 0);
      result->type = type; 
      result->cdr.cons = ptr;
      break; 
    } 
   
  if (head_is_symbol) {
    /* potential function application */ 
    /* hack */ 
    if ( result->car.s == 15438 ) {
      
      cons_t *fun_res;

      fun_res = bi_add(result->cdr.cons); 
      return fun_res;
    }
  }

  return result;
}
  

