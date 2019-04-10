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

#include "symrepr.h"
#include "heap.h"
#include "builtin.h"
#include "print.h"
#include "env.h"
#include "stack.h"

#ifdef VISUALIZE_HEAP
#include "heap_vis.h"
#endif

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

#define EVAL_CONTINUE     1
#define PERFORM_GC        2

#define DONE              1
#define SET_GLOBAL_ENV    2
#define FUNCTION_APP      3
#define CLOSURE_APP       4
#define EVAL_REST         5
#define FUNCTION          6
#define BIND_TO_KEY_REST  7
#define IF                8

#define CONTINUE_EVAL(EXP,ENV) (curr_exp = (EXP), curr_env = (ENV), longjmp(rewind_buf,EVAL_CONTINUE))
#define GC_ON_ERROR(RES, ALLOCATION) ((RES) = (ALLOCATION), (type_of((RES)) == VAL_TYPE_SYMBOL && dec_sym((RES)) == symrepr_merror()) ? (longjmp(rewind_buf, PERFORM_GC), 0) : (RES))

static val_t run_eval(val_t orig_prg, val_t lisp, val_t env);
static val_t dispatch_continuation(val_t ix, val_t args);

jmp_buf rewind_buf;

// Removed volatile qualifier on these. 
static val_t curr_exp;
static val_t curr_env;
static val_t eval_cps_global_env;

static val_t NIL;

stack *K; // Stack describes the current continuation.
stack *K_save; // Stack save area for resume after gc.

val_t eval_cps_get_env(void) {
  return eval_cps_global_env;
}

val_t eval_cps_bi(val_t lisp) {
  CONTINUE_EVAL(car(lisp),curr_env);
}

// ////////////////////////////////////////////////////////
// Continuation points and apply cont
// ////////////////////////////////////////////////////////

val_t apply_continuation(stack *K){
  
  val_t k;
  val_t args;
  pop_u32(K, &args);
  pop_u32(K, &k);
  return dispatch_continuation(k, args);
}

val_t cont_done(val_t arg) {
  return arg;
}

val_t cont_set_global_env(val_t val){

  val_t curr = eval_cps_global_env;
  val_t tmp;
  val_t key;
  pop_u32(K,&key);

  while(type_of(curr) == PTR_TYPE_CONS) {
    if (car(car(curr)) == key) {
      set_cdr(car(curr),val);

      return enc_sym(symrepr_true());
    }
    curr = cdr(curr);
  }
  val_t keyval;
  GC_ON_ERROR(keyval, cons(key,val));
 
  GC_ON_ERROR(tmp, cons(keyval, eval_cps_global_env));

  eval_cps_global_env = tmp;
  return enc_sym(symrepr_true());
}


val_t cont_function_app(val_t args) {

  val_t args_rev;
  val_t fun;
  pop_u32(K, &fun);

  if (type_of(args) == PTR_TYPE_CONS) { // TODO: FIX THIS MESS
    GC_ON_ERROR(args_rev, reverse(args));
  } else {
    args_rev = args;
  }

  val_t (*f)(val_t) = builtin_lookup_function(dec_sym(fun));

  if (f == NULL) {
    return enc_sym(symrepr_eerror());
  }
  val_t f_res = f(args_rev);
  push_u32(K, f_res);
  return apply_continuation(K);
}

val_t cont_closure_app(val_t args) {

  val_t args_rev;
  val_t closure;
  pop_u32(K, &closure);

  if (type_of(args) == PTR_TYPE_CONS) { // TODO: FIX THIS MESS
    GC_ON_ERROR(args_rev, reverse(args));
  } else {
    args_rev = args;
  }

  val_t params  = car(cdr(closure));
  val_t exp     = car(cdr(cdr(closure)));
  val_t clo_env = car(cdr(cdr(cdr(closure))));

  // TODO: env_build_params_args should follow the same interface as the rest.
  //       Return symbol error on failure.
  val_t local_env;
  if (!env_build_params_args(params, args_rev, clo_env, &local_env)) {
    longjmp(rewind_buf, PERFORM_GC);
  }

  CONTINUE_EVAL(exp,local_env);
}

val_t cont_eval_rest(val_t head) {

  val_t rest;
  val_t acc;
  val_t env;

  pop_u32(K, &rest);
  pop_u32(K, &acc);
  pop_u32(K, &env);

  if (type_of(rest) == VAL_TYPE_SYMBOL &&
      rest == NIL) {

    val_t args;
    GC_ON_ERROR(args, cons(head,acc));

    push_u32(K, args);
    return apply_continuation(K);
  }

  GC_ON_ERROR(acc, cons(head, acc));

  push_u32(K, env);
  push_u32(K, acc);
  push_u32(K, cdr(rest));
  push_u32(K, enc_u28(EVAL_REST));

  CONTINUE_EVAL(car(rest),env);
}

// Closure or built-in function
val_t cont_function(val_t fun) {

  val_t fun_args;
  val_t env;
  pop_u32(K,&fun_args);
  pop_u32(K,&env);

  val_t head = car(fun_args);

  push_u32(K,fun);
  if ( type_of(fun) == PTR_TYPE_CONS &&
       dec_sym(car(fun)) == symrepr_closure()) {
    push_u32(K, enc_u28(CLOSURE_APP));
  } else {
    push_u32(K, enc_u28(FUNCTION_APP));
  }
  // If args are a list with at least one element, process the elements
  if (type_of(fun_args) == PTR_TYPE_CONS &&
      length(fun_args) >= 1) {
    push_u32(K,env);
    push_u32(K,NIL);
    push_u32(K,cdr(fun_args));
    push_u32(K, enc_u28(EVAL_REST));

    CONTINUE_EVAL(head,env);
  }
  // otherwise the arguments are an empty list (or something bad happened)
  push_u32(K, NIL);
  return apply_continuation(K);
}


val_t cont_bind_to_key_rest(val_t val) {
  val_t key;
  val_t env;
  val_t rest;

  pop_u32(K, &key);
  pop_u32(K, &env);
  pop_u32(K, &rest);

  env_modify_binding(env, key, val);

  if ( type_of(rest) == PTR_TYPE_CONS ){
    val_t keyn = car(car(rest));
    val_t valn_exp = car(cdr(car(rest)));

    push_u32(K,cdr(rest));
    push_u32(K,env);
    push_u32(K,keyn);
    push_u32(K, enc_u28(BIND_TO_KEY_REST));

    CONTINUE_EVAL(valn_exp,env);
  }

  // Otherwise evaluate the expression in the populated env
  val_t exp;
  pop_u32(K, &exp);

  CONTINUE_EVAL(exp,env);
}

val_t cont_if(val_t cond) {

  val_t then_branch;
  val_t else_branch;

  pop_u32(K, &then_branch);
  pop_u32(K, &else_branch);

  if (type_of(cond) == VAL_TYPE_SYMBOL &&
      dec_sym(cond) == symrepr_true()) {
    CONTINUE_EVAL(then_branch,curr_env);
  } else {
    CONTINUE_EVAL(else_branch,curr_env);
  }
}

val_t dispatch_continuation(val_t ix, val_t args) {

  switch(dec_u28(ix)) {
  case DONE:
    return cont_done(args);
    break;
  case SET_GLOBAL_ENV:
    return cont_set_global_env(args);
    break;
  case FUNCTION_APP:
    return cont_function_app(args);
    break;
  case CLOSURE_APP:
    return cont_closure_app(args);
    break;
  case EVAL_REST:
    return cont_eval_rest(args);
    break;
  case FUNCTION:
    return cont_function(args);
    break;
  case BIND_TO_KEY_REST:
    return cont_bind_to_key_rest(args);
    break;
  case IF:
    return cont_if(args);
    break;
  }
  return enc_sym(symrepr_eerror());
}

// ////////////////////////////////////////////////////////
// Broken out helpers 
// ////////////////////////////////////////////////////////

val_t process_let(val_t binds, val_t orig_env, val_t exp) {
  val_t curr = binds;
  val_t new_env = orig_env;

  if (type_of(binds) != PTR_TYPE_CONS) {
    return enc_sym(symrepr_eerror());
  }

  while (type_of(curr) == PTR_TYPE_CONS) {
    val_t key = car(car(curr));
    val_t val = NIL; // a temporary
    val_t binding; 
    GC_ON_ERROR(binding, cons(key,val));

    GC_ON_ERROR(new_env, cons(binding, new_env));

    curr = cdr(curr);
  }

  val_t key0 = car(car(binds));
  val_t val0_exp = car(cdr(car(binds)));

  push_u32(K,exp); // exp to evaluate in new environment
  push_u32(K,cdr(binds));
  push_u32(K,new_env);
  push_u32(K,key0);
  push_u32(K, enc_u28(BIND_TO_KEY_REST));

  CONTINUE_EVAL(val0_exp,new_env);
}

// ////////////////////////////////////////////////////////
// EVALUATION
// ////////////////////////////////////////////////////////
val_t eval_cps(val_t lisp, val_t env) {

  val_t head;
  val_t value = enc_sym(symrepr_eerror());

  switch (type_of(lisp)) {

  case VAL_TYPE_SYMBOL:
    if (!env_lookup(lisp, env, &value)) {
      if (!env_lookup(lisp, eval_cps_global_env, &value)){
	return enc_sym(symrepr_eerror());
      }
    }
    push_u32(K, value);
    return apply_continuation(K);
    
  case PTR_TYPE_F32:
  case PTR_TYPE_U32:
  case VAL_TYPE_I28:
  case PTR_TYPE_I32:
  case VAL_TYPE_CHAR:
  case VAL_TYPE_U28:
  case PTR_TYPE_ARRAY:
    push_u32(K, lisp);
    return apply_continuation(K);

  case PTR_TYPE_REF:
  case PTR_TYPE_STREAM:
    return enc_sym(symrepr_eerror());
    break;

  case PTR_TYPE_CONS:
    head = car(lisp);

    if (type_of(head) == VAL_TYPE_SYMBOL) {

      // Special form: QUOTE
      if (dec_sym(head) == symrepr_quote()) {
	value =  car(cdr(lisp));
	push_u32(K, value);
	return apply_continuation(K);
      }

      // Special form: DEFINE
      if (dec_sym(head) == symrepr_define()) {
	val_t key = car(cdr(lisp));
	val_t val_exp = car(cdr(cdr(lisp)));

	if (type_of(key) != VAL_TYPE_SYMBOL ||
	    key == NIL) {
	  return enc_sym(symrepr_eerror());
	}

	push_u32(K, key);
	push_u32(K, enc_u28(SET_GLOBAL_ENV));

	CONTINUE_EVAL(val_exp,env);
      }

      // Special form: LAMBDA
      if (dec_sym(head) == symrepr_lambda()) {
	val_t env_cpy;
	if (!env_copy_shallow(env,&env_cpy)) {
	    longjmp(rewind_buf, PERFORM_GC);
	  }

	val_t env_end = cons(env_cpy,NIL);
	val_t body    = cons(car(cdr(cdr(lisp))), env_end);
	val_t params  = cons(car(cdr(lisp)), body);
	val_t closure = cons(enc_sym(symrepr_closure()), params);

	if (type_of(env_end) == VAL_TYPE_SYMBOL ||
	    type_of(body)    == VAL_TYPE_SYMBOL ||
	    type_of(params)  == VAL_TYPE_SYMBOL ||
	    type_of(closure) == VAL_TYPE_SYMBOL) {
	  longjmp(rewind_buf, PERFORM_GC);
	}

	push_u32(K,closure);
	return apply_continuation(K);
      }

      // Special form: IF
      if (dec_sym(head) == symrepr_if()) {

	push_u32(K,car(cdr(cdr(cdr(lisp))))); // else branch
	push_u32(K,car(cdr(cdr(lisp)))); // Then branch
	push_u32(K, enc_u28(IF));

	CONTINUE_EVAL(car(cdr(lisp)),curr_env);
      }

      // Special form: LET
      if (dec_sym(head) == symrepr_let()) {
	val_t orig_env = env;
	val_t binds   = car(cdr(lisp)); // key value pairs.
	val_t exp     = car(cdr(cdr(lisp))); // exp to evaluate in the new env.
	// Setup the bindings
	return process_let(binds, orig_env, exp);
      }
    } // If head is symbol

    // Possibly a function application form:
    push_u32(K, curr_env);  // The environment each element should be evaluated in 
    push_u32(K, cdr(lisp)); // list of arguments that needs to be evaluated.
    push_u32(K, enc_u28(FUNCTION));

    CONTINUE_EVAL(head,curr_env);

  default:
    // BUG No applicable case!
    return enc_sym(symrepr_eerror());
    break;
  }
}


val_t run_eval(val_t orig_prg, val_t lisp, val_t env){

  push_u32(K, enc_u28(DONE));
  push_u32(K_save, enc_u28(DONE));

  curr_exp = lisp;
  curr_env = env;

  val_t r;

  int res = setjmp(rewind_buf);

  if (res) {

#ifdef VISUALIZE_HEAP
    heap_vis_gen_image();
#endif
    if (res == PERFORM_GC) {
      clear_stack(K);
      copy_stack(K, K_save);

      heap_perform_gc_aux(eval_cps_global_env, curr_env, curr_exp, orig_prg, K->data, K->sp);
    }

    clear_stack(K_save);
    copy_stack(K_save, K);

    r = eval_cps(curr_exp, curr_env);

  } else {

    // kickstarts evaluation with the done_cont;
    r = eval_cps(curr_exp, curr_env);
  }

  if (type_of(r) == VAL_TYPE_SYMBOL &&
      (dec_sym(r) == symrepr_eerror() ||
       dec_sym(r) == symrepr_merror() ||
       dec_sym(r) == symrepr_terror())) {
    clear_stack(K);
    clear_stack(K_save);
  }

  return r;
}

val_t eval_cps_program(val_t lisp) {

  val_t res = NIL;
  val_t local_env = NIL;
  val_t curr = lisp;

  while (type_of(curr) == PTR_TYPE_CONS) {
    res =  run_eval(lisp, car(curr),local_env);
    curr = cdr(curr);
  }
  return res;
}



int eval_cps_init() {

  K = init_cont_stack(1000);
  K_save = init_cont_stack(1000);

  int res = builtin_add_function("eval",eval_cps_bi);

  NIL = enc_sym(symrepr_nil());
  
  eval_cps_global_env = NIL;

  eval_cps_global_env = built_in_gen_env();

  val_t nil_entry = cons(NIL, NIL);
  eval_cps_global_env = cons(nil_entry, eval_cps_global_env);

  if (type_of(nil_entry) == VAL_TYPE_SYMBOL ||
      type_of(eval_cps_global_env) == VAL_TYPE_SYMBOL) res = 0;

  return res;
}
