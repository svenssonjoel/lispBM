
#include "eval.h"

#include "symrepr.h"
#include "heap.h"
#include "builtin.h"

#include <stdio.h>
#include <stdint.h> 


uint32_t evlis(uint32_t ptcons, uint32_t env);
uint32_t apply(uint32_t closure, uint32_t args); 
uint32_t apply_builtin(uint32_t sym, uint32_t args); 

uint32_t eval_program(uint32_t lisp, uint32_t env) {
  // Program is a list of expressions that should be evaluated individually 
  if ( IS_PTR(lisp) &&
       PTR_TYPE(lisp) == PTR_TYPE_CONS) {
    // environment should be updated... 
    return cons(eval(car(lisp), env), eval_program(cdr(lisp),env)); 
  }
  
  return eval(lisp,env);
} 

uint32_t eval(uint32_t lisp, uint32_t env) {
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
      return apply(eval(car(lisp),env),
		   evlis(cdr(lisp), env)); 
    } else {
      return apply_builtin(eval(car(lisp),env),
			   evlis(cdr(lisp),env)); 
    }
    break;
  default:
    // TODO:
    return ENC_SYM(symrepr_eerror());
    break; 

  }
  
  // TODO: Bottoming out here should not happen
  return ENC_SYM(symrepr_eerror());
} 

// takes a ptr to cons and returns a ptr to cons.. 
uint32_t evlis(uint32_t pcons, uint32_t env) {
  if ( IS_PTR(pcons) &&
       PTR_TYPE(pcons) == PTR_TYPE_CONS) {
    return cons(eval(car(pcons), env), evlis(cdr(pcons),env)); 
  }
  return eval(pcons,env); 
}

uint32_t apply(uint32_t closure, uint32_t args) {
  printf("apply\n");
  return ENC_SYM(symrepr_nil()); 
}

uint32_t apply_builtin(uint32_t sym, uint32_t args) {

  uint32_t (*f)(uint32_t) = builtin_lookup_function(DEC_SYM(sym));

  if (f == NULL)
    return ENC_SYM(symrepr_eerror());

  return f(args); 
  /*
  if (sym == ENC_SYM(symrepr_plus())) {
    uint32_t tmp = args; 
    uint32_t sum = 0; 
    while ( tmp != ENC_SYM(symrepr_nil())) {
      uint32_t v = car(tmp);
      sum += DEC_I28(v);

      tmp = cdr(tmp);
    }
    return ENC_I28(sum); 
  } else if (sym == ENC_SYM(symrepr_mult())) {
    
  }
  return ENC_SYM(symrepr_eerror()); 
  */
}
