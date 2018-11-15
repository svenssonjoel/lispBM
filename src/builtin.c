
#include <stdio.h>
#include <stdlib.h> 

#include "symrepr.h"
#include "heap.h"
#include "builtin.h"


typedef struct s_builtin_function{
  uint32_t sym;
  bi_fptr fun_ptr;
  struct s_builtin_function* next; 
} builtin_function_t; 

builtin_function_t* function_list = NULL;

// TODO: Implement a small cache for common lookups.

// Built in functions

uint32_t bi_fun_sum(uint32_t args) { // TODO: typechecking and potential conversion
  uint32_t tmp = args;
  int32_t sum = 0;
  while ( DEC_SYM(tmp) != symrepr_nil()) {
    int32_t v = car(tmp);
    sum += DEC_I28(v);
    tmp = cdr(tmp); 
  }
  return ENC_I28(sum);
}

// Interface functions

bi_fptr builtin_lookup_function(uint32_t sym){
  builtin_function_t *t = function_list; 
  
  while (t != NULL) {
    if ( t->sym == sym ) {
      return t->fun_ptr;
    }
    t = t->next; 
  }
  return NULL; 
}

int builtin_add_function(char *sym_str, bi_fptr fun_ptr){

  uint32_t symbol;
  int res = symrepr_addsym(sym_str, &symbol);
  
  if ( !res ) {
    return 0;
  }

  builtin_function_t *bi = malloc(sizeof(builtin_function_t));

  if ( !bi ) return 0;

  bi->sym = symbol;
  bi->fun_ptr = fun_ptr;
  bi->next = function_list;

  function_list = bi;
  return 1; 
} 

int builtin_init(void) {
  int res = 1;

  res &= builtin_add_function("+", bi_fun_sum); 

  return res; 
}
