

#include "symtab.h"
#include "heap0.h" 

#define NUM_BUILT_IN 5

char *bi_names[NUM_BUILT_IN] =
  { "+",
    "-",
    "*",
    "/",
    "DEFINE", /* store on heap and link from environment */ 
  };

/* prime the symtab with built-in names. */ 
int built_in_init(void) {
  int i;
  int r;
  uint32_t id; /* throw away */ 
  for (i = 0; i < NUM_BUILT_IN; i ++) {
    r = symtab_addname(bi_names[i],&id);
    if (!r) return 0;
  }
  return 1; 
}

cons_t *bi_add(cons_t *args) {
  cons_t* res = heap_allocate_cell();
  int32_t acc = 0; ;
  float   acc_f = 0; 
  cons_t* head = args;
  
  
  while(head) { /* Do this more rigorously */ 
    if (GET_CAR_TYPE(head->type) == INTEGER &&
	GET_CDR_TYPE(head->type) == POINTER) {
      
      acc += head->car.i; 

      head = head->cdr.cons;
    } else {
      return NULL;
    }
  }
  res->type = (NIL << 8 | INTEGER);
  res->car.i = acc;
  res->cdr.i = 0; /* hack */

  return res; 
}
