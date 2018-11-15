
#include "eval.h"

#include "symrepr.h"
#include "heap.h"
#include "builtin.h"

#include <stdio.h>
#include <stdint.h> 

static uint32_t evlis(uint32_t ptcons, uint32_t env);
static uint32_t apply(uint32_t closure, uint32_t args); 
static uint32_t apply_builtin(uint32_t sym, uint32_t args); 
static uint32_t eval_in_env(uint32_t, uint32_t); 

static uint32_t global_env; 

uint32_t eval_bi(uint32_t lisp) {
  return eval_in_env(car(lisp),ENC_SYM(symrepr_nil()));
}

int eval_init() {
  global_env = ENC_SYM(symrepr_nil());

  return builtin_add_function("eval", eval_bi);
}


uint32_t eval_program(uint32_t lisp) {
  // Program is a list of expressions that should be evaluated individually
  uint32_t local_env = ENC_SYM(symrepr_nil());
  if ( IS_PTR(lisp) &&
       PTR_TYPE(lisp) == PTR_TYPE_CONS) {
    // environment should be updated... 
    return cons(eval_in_env(car(lisp),local_env), eval_program(cdr(lisp))); 
  }
  return eval_in_env(lisp,local_env);
} 

uint32_t eval_in_env(uint32_t lisp, uint32_t env) {
  uint32_t nil = symrepr_nil();

  if (! IS_PTR(lisp)) {
    switch (VAL_TYPE(lisp)){

    case VAL_TYPE_SYMBOL:
      // lookup
      return lisp;
      break;
    default:
      return lisp; // cannot be evaluated further.
    }
  }

  uint32_t car_val;
  switch (PTR_TYPE(lisp)) {
  case PTR_TYPE_CONS:
    car_val = car(lisp); 
    // Check for special forms. quote, lambda, cond
    if (car_val == ENC_SYM(symrepr_quote())) {
      return (car (cdr (lisp)));
    } else if (car_val == ENC_SYM(symrepr_lambda())) {
      // deal with lambda
      return ENC_SYM(symrepr_nil()); 
    } else if (car_val == ENC_SYM(symrepr_closure())) {
      return apply(eval_in_env(car(lisp),env),
		   evlis(cdr(lisp), env)); 
    } else {
      return apply_builtin(eval_in_env(car(lisp),env),
			   evlis(cdr(lisp),env)); 
    }
    break;

    // TODO: All other ptr cases. Float etc.
    
  default: 
    return ENC_SYM(symrepr_eerror());
    break; 

  }
  
  // TODO: Bottoming out here should not happen
  return ENC_SYM(symrepr_eerror());
} 

// takes a ptr to cons and returns a ptr to cons.. 
static uint32_t evlis(uint32_t pcons, uint32_t env) {
  if ( IS_PTR(pcons) &&
       PTR_TYPE(pcons) == PTR_TYPE_CONS) {
    return cons(eval_in_env(car(pcons), env), evlis(cdr(pcons),env)); 
  }
  if (VAL_TYPE(pcons) == VAL_TYPE_SYMBOL &&
      pcons == ENC_SYM(symrepr_nil())) {
    return ENC_SYM(symrepr_nil());
  }
  printf("bad case\n");
  return ENC_SYM(symrepr_eerror());
}

static uint32_t apply(uint32_t closure, uint32_t args) {
  printf("apply\n");
  return ENC_SYM(symrepr_nil()); 
}

static uint32_t apply_builtin(uint32_t sym, uint32_t args) {

  uint32_t (*f)(uint32_t) = builtin_lookup_function(DEC_SYM(sym));

  if (f == NULL) {
    printf("NULL %d\n", DEC_SYM(sym)); 
    return ENC_SYM(symrepr_eerror());
  }

  return f(args); 
}
