
#include "eval.h"

#include "symrepr.h"
#include "heap.h"
#include "builtin.h"
#include "print.h"

#include <stdio.h>
#include <stdint.h> 

static uint32_t evlis(uint32_t ptcons, uint32_t env);
static uint32_t apply(uint32_t closure, uint32_t args); 
static uint32_t apply_builtin(uint32_t sym, uint32_t args); 
static uint32_t eval_in_env(uint32_t, uint32_t); 

static uint32_t global_env;

void eval_set_env(uint32_t env) {
  global_env = env;
}
uint32_t eval_get_env(void) {
  return global_env;
}

uint32_t eval_bi(uint32_t lisp) {
  return eval_in_env(car(lisp),ENC_SYM(symrepr_nil()));
}

uint32_t define_bi(uint32_t lisp) {

  uint32_t key = car(lisp);
  uint32_t val = car(cdr(lisp));

  if (VAL_TYPE(key) != VAL_TYPE_SYMBOL)
    return ENC_SYM(symrepr_nil());

  if (DEC_SYM(key) == symrepr_nil())
    return ENC_SYM(symrepr_nil());
  
  uint32_t keyval = heap_allocate_cell();
  set_car(keyval, key);
  set_cdr(keyval, val);
  uint32_t entry  = heap_allocate_cell();
  set_car(entry, keyval);
  set_cdr(entry, global_env);
  global_env = entry;

  return ENC_SYM(symrepr_nil());
}

uint32_t lookup_global_env(uint32_t sym) {
  uint32_t curr = global_env; 
  
  while (IS_PTR(curr) && PTR_TYPE(curr) == PTR_TYPE_CONS) {

    if (car(car(curr)) == sym) {
      return cdr(car(curr));
    }
    curr = cdr(curr);
  }
  if (VAL_TYPE(curr) == VAL_TYPE_SYMBOL &&
      DEC_SYM(curr) == symrepr_nil()) {
    return curr; 
  }
  
  return ENC_SYM(symrepr_eerror()); 
}

int eval_init() {
  global_env = ENC_SYM(symrepr_nil());
  int res = 1;
  res &= builtin_add_function("eval", eval_bi);
  res &= builtin_add_function("define", define_bi);
  return res;
}


uint32_t eval_program(uint32_t lisp) {
  // Program is a list of expressions that should be evaluated individually
  uint32_t local_env = ENC_SYM(symrepr_nil());
  if ( IS_PTR(lisp) &&
       PTR_TYPE(lisp) == PTR_TYPE_CONS) {
    // environment should be updated...
    uint32_t car_val = eval_in_env(car(lisp),local_env);
    uint32_t cdr_val = eval_program(cdr(lisp)); 
    
    return cons(car_val, cdr_val);  
  }
  return eval_in_env(lisp,local_env);
} 

uint32_t eval_in_env(uint32_t lisp, uint32_t env) {
  uint32_t nil = symrepr_nil();
  uint32_t val = 0; 

  if (! IS_PTR(lisp)) {
    switch (VAL_TYPE(lisp)){

    case VAL_TYPE_SYMBOL:
      val = lookup_global_env(lisp);
      if ( VAL_TYPE(val) == VAL_TYPE_SYMBOL &&
	   DEC_SYM(val) == symrepr_nil()) {
	return ENC_SYM(symrepr_nil());
      } else {
	return val;
      }
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
    if (VAL_TYPE(car_val) == VAL_TYPE_SYMBOL &&
        DEC_SYM(car_val) == symrepr_quote()){ 
      return (car (cdr (lisp)));
    }
    
    if (VAL_TYPE(car_val) == VAL_TYPE_SYMBOL &&
	DEC_SYM(car_val) == symrepr_lambda()) {
       
      return cons(ENC_SYM(symrepr_closure()),
		  cons(car(cdr(lisp)),
		       cons(car(cdr(cdr(lisp))),
			    ENC_SYM(symrepr_nil()))));
    }

    uint32_t e_car_val = eval_in_env(car_val, env); 
    
    if (VAL_TYPE(e_car_val) == VAL_TYPE_SYMBOL){ 
      return apply_builtin(e_car_val, evlis(cdr(lisp),env));
    }
    
    // closure application case
    return apply(e_car_val, evlis(cdr(lisp), env));

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
      DEC_SYM(pcons) == symrepr_nil()) {
    return ENC_SYM(symrepr_nil());
  }
  printf("bad case\n");
  return ENC_SYM(symrepr_eerror());
}

static uint32_t apply(uint32_t closure, uint32_t args) {
  return ENC_SYM(symrepr_nil()); 
}

static uint32_t apply_builtin(uint32_t sym, uint32_t args) {

  uint32_t (*f)(uint32_t) = builtin_lookup_function(DEC_SYM(sym));

  if (f == NULL) {
    return ENC_SYM(symrepr_eerror());
  }

  return f(args); 
}
