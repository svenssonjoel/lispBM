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
#include "env.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

#define EVAL_ERROR_BUFFER_SIZE 2048
#define ERROR(...) do {\
  memset(eval_error_string, 0, EVAL_ERROR_BUFFER_SIZE);\
  snprintf(eval_error_string,EVAL_ERROR_BUFFER_SIZE, __VA_ARGS__);	\
  if (error_jmp_ptr){\
    longjmp(*error_jmp_ptr,1);\
  }\
  } while(0)

static uint32_t evlis(uint32_t ptcons, uint32_t env);
static uint32_t apply(uint32_t closure, uint32_t args); 
static uint32_t apply_builtin(uint32_t sym, uint32_t args); 
static uint32_t eval_in_env(uint32_t, uint32_t); 
static uint32_t eval_let_bindings(uint32_t, uint32_t);

static char eval_error_string[EVAL_ERROR_BUFFER_SIZE]; 

static jmp_buf error_jmp_buffer;
static jmp_buf *error_jmp_ptr = NULL; 

static uint32_t eval_global_env;

char *eval_get_error() {
  return eval_error_string;
}

uint32_t eval_get_env(void) {
  return eval_global_env;
}

uint32_t eval_bi(uint32_t lisp) {

  if (!error_jmp_ptr) {
    error_jmp_ptr = &error_jmp_buffer;

    if (setjmp(*error_jmp_ptr)) {
      error_jmp_ptr = 0;
      return ENC_SYM(symrepr_eerror()); 
    }
  }
  
  return eval_in_env(car(lisp),ENC_SYM(symrepr_nil()));
  
}


int eval_init() {

  int res = 1;
  res &= builtin_add_function("eval", eval_bi);
 
  eval_global_env = built_in_gen_env();

  uint32_t nil_entry = cons(ENC_SYM(symrepr_nil()), ENC_SYM(symrepr_nil()));
  eval_global_env = cons(nil_entry, eval_global_env);

  return res; 
  
}

uint32_t do_eval_program(uint32_t lisp) {
   
  // Program is a list of expressions that should be evaluated individually
  uint32_t res; 
  uint32_t local_env = ENC_SYM(symrepr_nil());

  if (TYPE_OF(lisp) == PTR_TYPE_CONS) {
    
    uint32_t car_val = eval_in_env(car(lisp),local_env);
    uint32_t cdr_val = eval_program(cdr(lisp)); 
    
    res = cons(car_val, cdr_val);  
  } else {
    res =  eval_in_env(lisp,local_env);
  }

  return res;   
}

uint32_t eval_program(uint32_t lisp) {

  // Setup jmp buffer for breaking out of recursion on error.
  if (!error_jmp_ptr) {
    error_jmp_ptr = &error_jmp_buffer; 

    if (setjmp(*error_jmp_ptr)) {
      error_jmp_ptr = 0;
      return ENC_SYM(symrepr_eerror());
    }
  }
  
  uint32_t res = do_eval_program(lisp);
  
  if (error_jmp_ptr) error_jmp_ptr = 0;
  return res; 
}

uint32_t eval_in_env(uint32_t lisp, uint32_t env) {

  uint32_t tmp = 0;
  int ret;
  uint32_t head;
  
  switch(TYPE_OF(lisp)) {
  case VAL_TYPE_SYMBOL:
    ret = env_lookup(lisp, env, &tmp);
    if (!ret) {
      ret = env_lookup(lisp, eval_global_env, &tmp);
    }
    if (ret) return tmp;
    ERROR("Eval: Variable lookup failed: %s ",symrepr_lookup_name(DEC_SYM(lisp)));
    break;
  case VAL_TYPE_I28:
  case VAL_TYPE_U28:
  case VAL_TYPE_CHAR:
    return lisp;    
    break;
  case PTR_TYPE_CONS:
    head = car(lisp);

    if (TYPE_OF(head) == VAL_TYPE_SYMBOL) {

      // Special form: QUOTE
      if (DEC_SYM(head) == symrepr_quote()) {

	return (car (cdr (lisp)));
      }

      // Special form: LAMBDA
      if (DEC_SYM(head) == symrepr_lambda()) {

	uint32_t env_cpy;
	if (!env_copy_shallow(env, &env_cpy))
	  ERROR("OUT OF MEMORY"); 
	return cons(ENC_SYM(symrepr_closure()),
		    cons(car(cdr(lisp)),
			 cons(car(cdr(cdr(lisp))),
			      cons(env_cpy, ENC_SYM(symrepr_nil())))));
      }

      // Special form: IF
      if (DEC_SYM(head) == symrepr_if()) {

	uint32_t pred_res = eval_in_env(car(cdr(lisp)), env);
	if (VAL_TYPE(pred_res) == VAL_TYPE_SYMBOL &&
	    DEC_SYM(pred_res) == symrepr_true()) {
	  return eval_in_env(car(cdr(cdr(lisp))), env);
	} else {
	  // TODO: CHECK THAT IS NOT A PROGRAMMER ERROR
	  return eval_in_env(car(cdr(cdr(cdr(lisp)))), env);
	}
      }

      // Special form: DEFINE
      if (DEC_SYM(head) == symrepr_define()) {
	uint32_t key = car(cdr(lisp));
	uint32_t val = eval_in_env(car(cdr(cdr(lisp))), env);
	uint32_t curr = eval_global_env; 

	if (TYPE_OF(key) != VAL_TYPE_SYMBOL)
	  ERROR("Define expects a symbol");

	if (DEC_SYM(key) == symrepr_nil())
	  ERROR("Cannot redefine nil");

	while(TYPE_OF(curr) == PTR_TYPE_CONS) {
	  if (car(car(curr)) == key) {
	    set_cdr(car(curr),val);
	    return ENC_SYM(symrepr_true());
	  }
	  curr = cdr(curr);
	}
	uint32_t keyval = cons(key,val);
	eval_global_env = cons(keyval,eval_global_env);
	return ENC_SYM(symrepr_true()); 	
      }

      // Special form: LET
      if (DEC_SYM(head) == symrepr_let()) {

	uint32_t new_env = eval_let_bindings(car(cdr(lisp)),env);
	return eval_in_env(car(cdr(cdr(lisp))),new_env);
      }

    } // If head is symbol
    
    // Possibly an application form:

    uint32_t head_val = eval_in_env(head, env); 
  
    if (TYPE_OF(head_val) == VAL_TYPE_SYMBOL) {
      return apply_builtin(head_val, evlis(cdr(lisp),env));
    }
    
    // Possibly a closure application (or programmer error)
    return apply(head_val, evlis(cdr(lisp), env));
    
    break;
  case PTR_TYPE_I32:
  case PTR_TYPE_U32:
  case PTR_TYPE_F32:
    return lisp;
    break;
  case PTR_TYPE_ARRAY:
    return lisp;
    break;
  case PTR_TYPE_REF:
  case PTR_TYPE_STREAM:
    ERROR("Arrays, refs and streams not implemented");
    break;
  default:
    ERROR("BUG! Fell through all cases in eval.");
    break;
  }

  ERROR("BUG! This cannot happen!");
  return ENC_SYM(symrepr_eerror());
} 

static uint32_t apply(uint32_t closure, uint32_t args) {

  // TODO: error checking etc
  //uint32_t clo_sym = car(closure);      
  uint32_t params  = car(cdr(closure)); // parameter list
  uint32_t exp     = car(cdr(cdr(closure)));
  uint32_t clo_env = car(cdr(cdr(cdr(closure))));

  uint32_t local_env;
  if (!env_build_params_args(params, args, clo_env, &local_env))
    ERROR("Could not create local environment"); 
  //printf("CLOSURE ENV: "); simple_print(local_env); printf("\n"); 
  
  return eval_in_env(exp,local_env);
}

// takes a ptr to cons and returns a ptr to cons.. 
static uint32_t evlis(uint32_t pcons, uint32_t env) {

  if (TYPE_OF(pcons) == PTR_TYPE_CONS) { 
    return cons(eval_in_env(car(pcons), env),
		evlis(cdr(pcons),env)); 
  }

  if (TYPE_OF(pcons) == VAL_TYPE_SYMBOL &&
      DEC_SYM(pcons) == symrepr_nil()) {
    return ENC_SYM(symrepr_nil());
  }

  ERROR("Evlis argument is not a list"); 
  return ENC_SYM(symrepr_eerror());
}

static uint32_t eval_let_bindings(uint32_t bind_list, uint32_t env) {

  uint32_t new_env = env;
  uint32_t curr = bind_list; 
  int res; 
  
  //setup the bindings
  while (TYPE_OF(curr) == PTR_TYPE_CONS) { 
    uint32_t key = car(car(curr));
    uint32_t val = ENC_SYM(symrepr_nil()); // a temporary
    uint32_t binding = cons(key,val);
    new_env = cons(binding, new_env); 
    curr = cdr(curr); 
  }

  // evaluate the bodies
  curr = bind_list; 
  while (TYPE_OF(curr) == PTR_TYPE_CONS) {
    uint32_t key = car(car(curr));
    uint32_t val = eval_in_env(car(cdr(car(curr))),new_env);

    res = env_modify_binding(new_env, key, val);
    if (!res) ERROR("Unable to modify letrec bindings");
    curr = cdr(curr); 
  }

  return new_env;
}


static uint32_t apply_builtin(uint32_t sym, uint32_t args) {

  uint32_t (*f)(uint32_t) = builtin_lookup_function(DEC_SYM(sym));

  if (f == NULL) {
    ERROR("Built in function does not exist"); 
    //return ENC_SYM(symrepr_eerror());
  }

  return f(args); 
}
