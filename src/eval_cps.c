
#include "symrepr.h"
#include "heap.h"
#include "builtin.h"
#include "print.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

extern int lookup_env(uint32_t sym, uint32_t env, uint32_t *res);

/* 
   Attempt to implement an evaluator in continuation passing style: 
   (In spite of C not being very suitable for the task.) 
   
   - No idea if this will work..

   - I hope this is possible without C being able to generate functions
     on the fly (and without going all the way to some jit setup). 

   - peeking a lot at: http://lisperator.net/pltut/cps-evaluator/stack-guard
     during the implementation of this.
     
   - Want to rewind the stack using setjmp longjmp.

   - Memory management will be real tricky since cannot rely on data on the stack.

 */

jmp_buf rewind_buf;

typedef struct {
  uint8_t *(*fptr)(uint8_t *, uint8_t *);
  uint32_t args_bytes;
  uint8_t *args;
} cont;
    
static uint32_t global_env;

uint32_t eval_cps_get_env(void) {
  return global_env;
}

uint8_t *set_global_env(uint8_t *args0, uint8_t *args){

  uint32_t curr = global_env;
  uint32_t *res = malloc(sizeof(uint32_t));
  
  uint32_t key = *(uint32_t*)args0;
  uint32_t val = *(uint32_t*)args;

  printf("set_global_env Key: %u |", key); simple_print(key);printf("\n"); 

  while(TYPE_OF(curr) == PTR_TYPE_CONS) {
    if (car(car(curr)) == key) {
      set_cdr(car(curr),val);

      *res = ENC_SYM(symrepr_true());
      free(args);
      free(args0);
      return (uint8_t*)res;
    }
    curr = cdr(curr);
  }
  uint32_t keyval = cons(key,val);
  if (TYPE_OF(keyval) == VAL_TYPE_SYMBOL) {
    *res = ENC_SYM(symrepr_nil());
    free(args);
    free(args0);
    return (uint8_t*)res;
  }
  global_env = cons(keyval,global_env);
  
  *res = ENC_SYM(symrepr_true());
  free(args);
  free(args0);
  return (uint8_t*)res;
}


uint8_t *done(uint8_t *args0, uint8_t *args) {

  printf("done\n"); 
  // Evaluation always finishes with the computation of an uint32_t;
  uint32_t *res = malloc(sizeof(uint32_t));

  *res = *(uint32_t *)args;
  free(args);
  
  return (uint8_t*)res;
}

uint8_t *apply_continuation(cont k, uint8_t *args){

  printf("apply_continuation\n");

  uint8_t *r = (*k.fptr)(k.args, args);
  return r;
}

uint8_t *eval_cps(uint32_t *lisp_in, uint32_t *env_in,cont *k_in) {

  cont k = *k_in;
  uint32_t env = *env_in;
  uint32_t lisp = *lisp_in;
  
  uint32_t tmp = ENC_SYM(symrepr_eerror()); 
  int ret = 0;
  uint32_t head;
  
  printf("eval_cps\n"); 
  switch (TYPE_OF(lisp)) {

  case VAL_TYPE_SYMBOL:
    ret = lookup_env(lisp, env, &tmp);
    if (!ret) {
      ret = lookup_env(lisp, global_env, &tmp);
    }
    if (ret) {
      uint32_t *val = malloc(sizeof(uint32_t));
      *val = tmp;
      return apply_continuation(k, (uint8_t*)val);
    }
    uint32_t *val = malloc(sizeof(uint32_t));
    *val = ENC_SYM(symrepr_eerror()); 
    return (uint8_t*)val;
    
  case VAL_TYPE_I28: 
  case VAL_TYPE_CHAR:
  case VAL_TYPE_U28: {
    uint32_t *val = malloc(sizeof(uint32_t));
    *val = lisp;
    return apply_continuation(k, (uint8_t*)val);
  }

  case PTR_TYPE_CONS:
    head = car(lisp);


    if (TYPE_OF(head) == VAL_TYPE_SYMBOL) {

      if (DEC_SYM(head) == symrepr_define()) {
	uint32_t key = car(cdr(lisp));
	printf("eval_cps Key: %u |", key); simple_print(key);printf("\n"); 
	uint32_t val_exp = car(cdr(cdr(lisp)));

	if (TYPE_OF(key) != VAL_TYPE_SYMBOL ||
	    DEC_SYM(key) == symrepr_nil()) {
	  uint32_t *val = malloc(sizeof(uint32_t));
	  *val = ENC_SYM(symrepr_eerror()); 
	  return (uint8_t*)val;
	}

	cont define_cont;
	define_cont.fptr = set_global_env;
	define_cont.args_bytes = sizeof(uint32_t);
	define_cont.args = malloc(sizeof(uint32_t));
	memcpy(define_cont.args,&key, sizeof(uint32_t));

	*lisp_in = val_exp;
	*env_in = env;
	*k_in = define_cont;
	longjmp(rewind_buf,1); 
	//return eval_cps(val_exp, env, define_cont);
      }
    }
  }
}
  
int run_eval(uint32_t lisp, uint32_t env) {

  cont done_cont;
  done_cont.fptr = done;
  done_cont.args = NULL;
  done_cont.args_bytes = 0; 

  volatile uint32_t curr_exp = lisp;
  volatile uint32_t curr_env = env;
  volatile cont     curr_cont = done_cont;  

  uint8_t *r;
  
  printf("run_eval\n"); 
  
  if (setjmp(rewind_buf)) {
    printf("Rewind!\n");

    // This is a good place to do periodic garbage collection
    // TODO: Check number of free cells and conditionally perform gc 
    
    r = eval_cps(&curr_exp, &curr_env, &curr_cont);
   	
  } else {

    // kickstarts evaluation with the done_cont;
    r = eval_cps(&curr_exp, &curr_env, &curr_cont);
  
  }

  // DONE!
  uint32_t r_val = *(uint32_t*)r;  
  simple_print(r_val); printf("\n");

  return 1; 
}


int eval_cps_init() {

  global_env = ENC_SYM(symrepr_nil());

  uint32_t nil_entry = cons(ENC_SYM(symrepr_nil()), ENC_SYM(symrepr_nil()));
  global_env = cons(nil_entry, global_env);

  
  return 1;
}

/*
uint32_t eval_cps(uint32_t lisp, uint32_t env) {

  jmp_buf rewind_buf;

  volatile uint32_t current_cont = lisp;
  volatile uint32_t current_env  = env;
  volatile uint32_t res = ENC_SYM(symrepr_nil());

  setjmp(rewind_buf);

  simple_print(current_cont);printf("\n");
  simple_print(current_env);printf("\n");
  simple_print(res);printf("\n"); 

  
  if (TYPE_OF(current_cont) == VAL_TYPE_SYMBOL &&
      DEC_SYM(current_cont) == symrepr_nil()) {
    return res;
  }
  if (DEC_I28(res) == 975) return res;

  current_cont = ENC_SYM(symrepr_eerror());
  current_env  = ENC_SYM(symrepr_eerror());
  res = ENC_I28(975); 

  longjmp(rewind_buf, 1); 
  


}
*/ 
