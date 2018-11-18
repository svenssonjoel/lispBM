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

uint32_t lookup_env(uint32_t sym, uint32_t env) {
  uint32_t curr = env;
  
  if(DEC_SYM(sym) == symrepr_nil())
    return sym;
  
  while (IS_PTR(curr) && PTR_TYPE(curr) == PTR_TYPE_CONS) {

    if (car(car(curr)) == sym) {
      return cdr(car(curr));
    }
    curr = cdr(curr);
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
      val = lookup_env(lisp,env);
      if ( VAL_TYPE(val) == VAL_TYPE_SYMBOL &&
	   DEC_SYM(val) == symrepr_eerror()) {
	val = lookup_env(lisp,global_env);
      }
      return val;
      
      break;
    default:
      return lisp; // cannot be evaluated further.
    }
  }

  uint32_t car_val;
  switch (PTR_TYPE(lisp)) {
  case PTR_TYPE_CONS:
    car_val = car(lisp); 

    // Special form: QUOTE 
    if (VAL_TYPE(car_val) == VAL_TYPE_SYMBOL &&
        DEC_SYM(car_val) == symrepr_quote()){ 
      return (car (cdr (lisp)));
    }

    // Special form: LAMBDA
    if (VAL_TYPE(car_val) == VAL_TYPE_SYMBOL &&
	DEC_SYM(car_val) == symrepr_lambda()) {
       
      return cons(ENC_SYM(symrepr_closure()),
		  cons(car(cdr(lisp)),
		       cons(car(cdr(cdr(lisp))),
			    ENC_SYM(symrepr_nil()))));
    }

    // Special form: COND
    if (VAL_TYPE(car_val) == VAL_TYPE_SYMBOL &&
	DEC_SYM(car_val) == symrepr_cond()) {
      printf("NOT IMPLEMENTED\n");
      return ENC_SYM(symrepr_nil()); 
    }
    
    // define and let could also be special forms.
    // Currently define is implemented as a built in function..

    
    // Possibly an application form 
    uint32_t e_car_val = eval_in_env(car_val, env); 
  
    if (!IS_PTR(e_car_val) && VAL_TYPE(e_car_val) == VAL_TYPE_SYMBOL) {
      return apply_builtin(e_car_val, evlis(cdr(lisp),env));
    }
    
    // Possibly a closure application (or programmer error)
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

uint32_t build_env_params_args(uint32_t params, uint32_t args) {
  uint32_t curr_param = params;
  uint32_t curr_arg = args;

  if (length(params) != length(args)) // programmer error
    return ENC_SYM(symrepr_nil()); 

  uint32_t env = ENC_SYM(symrepr_nil());
  while (IS_PTR(curr_param)) {

    uint32_t entry = cons(car(curr_param), car(curr_arg));
    env = cons(entry,env);
    
    curr_param = cdr(curr_param);
    curr_arg   = cdr(curr_arg); 
  }
  return env;
}

static uint32_t apply(uint32_t closure, uint32_t args) {

  // TODO: error checking etc
  uint32_t clo_sym = car(closure);      
  uint32_t params  = car(cdr(closure)); // parameter list
  uint32_t exp     = car(cdr(cdr(closure)));

  uint32_t local_env = build_env_params_args(params, args); 
  
  return eval_in_env(exp,local_env);
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

static uint32_t apply_builtin(uint32_t sym, uint32_t args) {

  uint32_t (*f)(uint32_t) = builtin_lookup_function(DEC_SYM(sym));

  if (f == NULL) {
    return ENC_SYM(symrepr_eerror());
  }

  return f(args); 
}
