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

/*
   Attempt to implement an evaluator in continuation passing style:
   (In spite of C not being very suitable for the task.)

   - No idea if this will work..
     # Seems to be. In the right direction at least.

   - I hope this is possible without C being able to generate functions
     on the fly (and without going all the way to some jit setup).
     # Looks like I was very incorrect in thinking anything like that
       would be needed.

   - peeking a lot at: http://lisperator.net/pltut/cps-evaluator/stack-guard
     during the implementation of this.
     # I still dont understand what goes on there.

   - Want to rewind the stack using setjmp longjmp.
     #Probably not essential. Or maybe it is..

   - Memory management will be real tricky since cannot rely on data on the stack.
     # Instead created another stack to depend upon.

   - Bah, this is too complicated and messy. Will try something new.

   - TODO: Something causes a list (nil nil) as arg to closure or func
           in a case where argument really should be just nil (at least this is my assumption). 
           Should not be GC related as it occurs on fresh heap.
	   It is also an error I introduced at some point while messing with
	   "on-demand" GC.

   - look at lispkit
*/

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

static uint32_t run_eval(uint32_t orig_prg, uint32_t lisp, uint32_t env);
static uint32_t dispatch_continuation(uint32_t ix, uint32_t args);

jmp_buf rewind_buf;

static volatile uint32_t curr_exp;
static volatile uint32_t curr_env;

static volatile uint32_t eval_cps_global_env;

stack *K; // Stack describes the current continuation.
stack *K_save; // Stack save area for resume after gc.

uint32_t eval_cps_get_env(void) {
  return eval_cps_global_env;
}

uint32_t eval_cps_bi(uint32_t lisp) {

  curr_exp = car(lisp);
  curr_env = curr_env;
  longjmp(rewind_buf,EVAL_CONTINUE);
}

// ////////////////////////////////////////////////////////
// Continuation points and apply cont
// ////////////////////////////////////////////////////////

uint32_t apply_continuation(stack *K){
  
  uint32_t k;
  uint32_t args;
  pop_u32(K, &args);
  pop_u32(K, &k);
  return dispatch_continuation(k, args);
}

uint32_t cont_done(uint32_t arg) {
  return arg;
}

uint32_t cont_set_global_env(uint32_t val){

  uint32_t curr = eval_cps_global_env;
  uint32_t tmp;
  uint32_t key;
  pop_u32(K,&key);

  while(TYPE_OF(curr) == PTR_TYPE_CONS) {
    if (car(car(curr)) == key) {
      set_cdr(car(curr),val);

      return ENC_SYM(symrepr_true());
    }
    curr = cdr(curr);
  }
  uint32_t keyval = cons(key,val);

  // check if cons was unsuccessful
  if (TYPE_OF(keyval) == VAL_TYPE_SYMBOL &&
      DEC_SYM(keyval) == symrepr_merror()) {
    // Abort computation and perform GC.
    longjmp(rewind_buf, PERFORM_GC);
  }
  tmp = cons(keyval,eval_cps_global_env);
  if (TYPE_OF(tmp) == VAL_TYPE_SYMBOL &&
      DEC_SYM(tmp) == symrepr_merror()) {
    longjmp(rewind_buf, PERFORM_GC);
  }

  eval_cps_global_env = tmp;
  return ENC_SYM(symrepr_true());
}


uint32_t cont_function_app(uint32_t args) {

  uint32_t args_rev;
  uint32_t fun;
  pop_u32(K, &fun);

  if (TYPE_OF(args) == PTR_TYPE_CONS) { // TODO: FIX THIS MESS
    args_rev = reverse(args);
    
    if (TYPE_OF(args_rev) == VAL_TYPE_SYMBOL &&
	DEC_SYM(args_rev) == symrepr_merror()) {
      longjmp(rewind_buf, PERFORM_GC);
    }
  } else {
    args_rev = args;
  }

  uint32_t (*f)(uint32_t) = builtin_lookup_function(DEC_SYM(fun));

  if (f == NULL) {
    return ENC_SYM(symrepr_eerror());
  }
  uint32_t f_res = f(args_rev);
  push_u32(K, f_res);
  return apply_continuation(K);
}

uint32_t cont_closure_app(uint32_t args) {

  uint32_t args_rev;
  uint32_t closure;
  pop_u32(K, &closure);

  if (TYPE_OF(args) == PTR_TYPE_CONS) { // TODO: FIX THIS MESS
    args_rev = reverse(args);
    if (TYPE_OF(args_rev) == VAL_TYPE_SYMBOL &&
	DEC_SYM(args_rev) == symrepr_merror()) {
      longjmp(rewind_buf, PERFORM_GC);
    }
  } else {
    args_rev = args;
  }

  uint32_t params  = car(cdr(closure));
  uint32_t exp     = car(cdr(cdr(closure)));
  uint32_t clo_env = car(cdr(cdr(cdr(closure))));

  uint32_t local_env;
  if (!env_build_params_args(params, args_rev, clo_env, &local_env)) {
    longjmp(rewind_buf, PERFORM_GC);
  }

  curr_exp = exp;
  curr_env = local_env;
  longjmp(rewind_buf, EVAL_CONTINUE);
}

uint32_t cont_eval_rest(uint32_t head) {

  uint32_t rest;
  uint32_t acc;
  uint32_t env;

  pop_u32(K, &rest);
  pop_u32(K, &acc);
  pop_u32(K, &env);

  if (TYPE_OF(rest) == VAL_TYPE_SYMBOL &&
      DEC_SYM(rest) == symrepr_nil()) {

    uint32_t args = cons(head, acc);
    if (TYPE_OF(args) == VAL_TYPE_SYMBOL &&
	DEC_SYM(args) == symrepr_merror()) {
      longjmp(rewind_buf, PERFORM_GC);
    }
    push_u32(K, args);
    return apply_continuation(K);
  }

  acc = cons(head, acc);
  if (TYPE_OF(acc) == VAL_TYPE_SYMBOL &&
      DEC_SYM(acc) == symrepr_merror()) {
    longjmp(rewind_buf, PERFORM_GC);
  }

  push_u32(K, env);
  push_u32(K, acc);
  push_u32(K, cdr(rest));
  push_u32(K, ENC_U28(EVAL_REST));

  curr_exp = car(rest);
  curr_env = env;
  longjmp(rewind_buf, EVAL_CONTINUE);
}

// Closure or built-in function
uint32_t cont_function(uint32_t fun) {

  uint32_t fun_args;
  uint32_t env;
  pop_u32(K,&fun_args);
  pop_u32(K,&env);

  uint32_t head = car(fun_args);

  push_u32(K,fun);
  if ( TYPE_OF(fun) == PTR_TYPE_CONS &&
       DEC_SYM(car(fun)) == symrepr_closure()) {
    push_u32(K, ENC_U28(CLOSURE_APP));
  } else {
    push_u32(K, ENC_U28(FUNCTION_APP));
  }
  // If args are a list with at least one element, process the elements
  if (TYPE_OF(fun_args) == PTR_TYPE_CONS &&
      length(fun_args) >= 1) {
    push_u32(K,env);
    push_u32(K,ENC_SYM(symrepr_nil()));
    push_u32(K,cdr(fun_args));
    push_u32(K, ENC_U28(EVAL_REST));

    curr_exp = head;
    curr_env = env;
    longjmp(rewind_buf, EVAL_CONTINUE);
  }
  // otherwise the arguments are an empty list (or something bad happened)
  push_u32(K, ENC_SYM(symrepr_nil()));
  return apply_continuation(K);
}


uint32_t cont_bind_to_key_rest(uint32_t val) {
  uint32_t key;
  uint32_t env;
  uint32_t rest;

  pop_u32(K, &key);
  pop_u32(K, &env);
  pop_u32(K, &rest);

  env_modify_binding(env, key, val);

  if ( TYPE_OF(rest) == PTR_TYPE_CONS ){
    uint32_t keyn = car(car(rest));
    uint32_t valn_exp = car(cdr(car(rest)));


    push_u32(K,cdr(rest));
    push_u32(K,env);
    push_u32(K,keyn);
    push_u32(K, ENC_U28(BIND_TO_KEY_REST));

    curr_exp = valn_exp;
    curr_env = env;
    longjmp(rewind_buf, EVAL_CONTINUE);
  }

  // Otherwise evaluate the expression in the populated env
  uint32_t exp;
  pop_u32(K, &exp);
  curr_exp = exp;
  curr_env = env;
  longjmp(rewind_buf, EVAL_CONTINUE);

}

uint32_t cont_if(uint32_t cond) {

  uint32_t then_branch;
  uint32_t else_branch;

  pop_u32(K, &then_branch);
  pop_u32(K, &else_branch);

  if (TYPE_OF(cond) == VAL_TYPE_SYMBOL &&
      DEC_SYM(cond) == symrepr_true()) {
    curr_exp = then_branch;
    //curr_env = curr_env;
    longjmp(rewind_buf,EVAL_CONTINUE);
  } else {
    curr_exp = else_branch;
    //curr_env = curr_env;
    longjmp(rewind_buf,EVAL_CONTINUE);
  }
}

uint32_t dispatch_continuation(uint32_t ix, uint32_t args) {

  switch(DEC_U28(ix)) {
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
  printf("Critical error\n");
  exit(0);
}

// ////////////////////////////////////////////////////////
// Broken out helpers 
// ////////////////////////////////////////////////////////

uint32_t process_let(uint32_t binds, uint32_t orig_env, uint32_t exp) {
  uint32_t curr = binds;
  uint32_t new_env = orig_env;

  if (TYPE_OF(binds) != PTR_TYPE_CONS) {
    return ENC_SYM(symrepr_eerror());
  }

  while (TYPE_OF(curr) == PTR_TYPE_CONS) {
    uint32_t key = car(car(curr));
    uint32_t val = ENC_SYM(symrepr_nil()); // a temporary
    uint32_t binding = cons(key,val);
    if (TYPE_OF(binding) == VAL_TYPE_SYMBOL &&
	DEC_SYM(binding) == symrepr_merror()) {
      longjmp(rewind_buf, PERFORM_GC);
    }
    new_env = cons(binding, new_env);
    if (TYPE_OF(new_env) == VAL_TYPE_SYMBOL &&
	DEC_SYM(new_env) == symrepr_merror()) {
      longjmp(rewind_buf, PERFORM_GC);
    }
    curr = cdr(curr);
  }

  uint32_t key0 = car(car(binds));
  uint32_t val0_exp = car(cdr(car(binds)));

  push_u32(K,exp); // exp to evaluate in new environment
  push_u32(K,cdr(binds));
  push_u32(K,new_env);
  push_u32(K,key0);
  push_u32(K, ENC_U28(BIND_TO_KEY_REST));

  curr_exp = val0_exp;
  curr_env = new_env;  // env annotated with temporaries
  longjmp(rewind_buf, EVAL_CONTINUE);
}

// ////////////////////////////////////////////////////////
// EVALUATION
// ////////////////////////////////////////////////////////
uint32_t eval_cps(uint32_t lisp, uint32_t env) {

  uint32_t head;
  uint32_t value = ENC_SYM(symrepr_eerror());

  switch (TYPE_OF(lisp)) {

  case VAL_TYPE_SYMBOL:
    if (!env_lookup(lisp, env, &value)) {
      if (!env_lookup(lisp, eval_cps_global_env, &value)){
	return ENC_SYM(symrepr_eerror());
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
    return ENC_SYM(symrepr_eerror());
    break;

  case PTR_TYPE_CONS:
    head = car(lisp);

    if (TYPE_OF(head) == VAL_TYPE_SYMBOL) {

      // Special form: QUOTE
      if (DEC_SYM(head) == symrepr_quote()) {
	value =  car(cdr(lisp));
	push_u32(K, value);
	return apply_continuation(K);
      }

      // Special form: DEFINE
      if (DEC_SYM(head) == symrepr_define()) {
	uint32_t key = car(cdr(lisp));
	uint32_t val_exp = car(cdr(cdr(lisp)));

	if (TYPE_OF(key) != VAL_TYPE_SYMBOL ||
	    DEC_SYM(key) == symrepr_nil()) {
	  return ENC_SYM(symrepr_eerror());
	}

	push_u32(K, key);
	push_u32(K, ENC_U28(SET_GLOBAL_ENV));

	curr_exp = val_exp;
	curr_env = env;
	longjmp(rewind_buf,EVAL_CONTINUE);
      }

      // Special form: LAMBDA
      if (DEC_SYM(head) == symrepr_lambda()) {
	uint32_t env_cpy;
	if (!env_copy_shallow(env,&env_cpy)) {
	    longjmp(rewind_buf, PERFORM_GC);
	  }

	uint32_t env_end = cons(env_cpy,ENC_SYM(symrepr_nil()));
	uint32_t body    = cons(car(cdr(cdr(lisp))), env_end);
	uint32_t params  = cons(car(cdr(lisp)), body);
	uint32_t closure = cons(ENC_SYM(symrepr_closure()), params);

	if (TYPE_OF(env_end) == VAL_TYPE_SYMBOL ||
	    TYPE_OF(body)    == VAL_TYPE_SYMBOL ||
	    TYPE_OF(params)  == VAL_TYPE_SYMBOL ||
	    TYPE_OF(closure) == VAL_TYPE_SYMBOL) {
	  longjmp(rewind_buf, PERFORM_GC);
	}

	push_u32(K,closure);
	return apply_continuation(K);
      }

      // Special form: IF
      if (DEC_SYM(head) == symrepr_if()) {

	push_u32(K,car(cdr(cdr(cdr(lisp))))); // else branch
	push_u32(K,car(cdr(cdr(lisp)))); // Then branch
	push_u32(K, ENC_U28(IF));

	curr_exp = car(cdr(lisp)); // condition
	curr_env = curr_env;
	longjmp(rewind_buf, EVAL_CONTINUE);

      }

      // Special form: LET
      if (DEC_SYM(head) == symrepr_let()) {
	uint32_t orig_env = env;
	uint32_t binds   = car(cdr(lisp)); // key value pairs.
	uint32_t exp     = car(cdr(cdr(lisp))); // exp to evaluate in the new env.
	// Setup the bindings
	return process_let(binds, orig_env, exp);
      }
    } // If head is symbol

    // Possibly an application form:
    push_u32(K, curr_env);  // The environment each element should be evaluated in 
    push_u32(K, cdr(lisp)); // list of arguments that needs to be evaluated.
    push_u32(K, ENC_U28(FUNCTION));

    curr_exp = head;
    curr_env = curr_env;
    longjmp(rewind_buf, EVAL_CONTINUE);

  default:
    // BUG No applicable case!
    return ENC_SYM(symrepr_eerror());
    break;
  }
}


uint32_t run_eval(uint32_t orig_prg, uint32_t lisp, uint32_t env){

  push_u32(K, ENC_U28(DONE));
  push_u32(K_save, ENC_U28(DONE));

  curr_exp = lisp;
  curr_env = env;

  uint32_t r;

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

  if (TYPE_OF(r) == VAL_TYPE_SYMBOL &&
      (DEC_SYM(r) == symrepr_eerror() ||
       DEC_SYM(r) == symrepr_merror() ||
       DEC_SYM(r) == symrepr_terror())) {
    clear_stack(K);
    clear_stack(K_save);
  }

  return r;
}

uint32_t eval_cps_program(uint32_t lisp) {

  uint32_t res = ENC_SYM(symrepr_nil());
  uint32_t local_env = ENC_SYM(symrepr_nil());
  uint32_t curr = lisp;

  while (TYPE_OF(curr) == PTR_TYPE_CONS) {
    res =  run_eval(lisp, car(curr),local_env);
    curr = cdr(curr);
  }
  return res;
}



int eval_cps_init() {

  K = init_cont_stack(1000);
  K_save = init_cont_stack(1000);

  int res = builtin_add_function("eval",eval_cps_bi);

  eval_cps_global_env = ENC_SYM(symrepr_nil());

  eval_cps_global_env = built_in_gen_env();

  uint32_t nil_entry = cons(ENC_SYM(symrepr_nil()), ENC_SYM(symrepr_nil()));
  eval_cps_global_env = cons(nil_entry, eval_cps_global_env);

  if (TYPE_OF(nil_entry) == VAL_TYPE_SYMBOL ||
      TYPE_OF(eval_cps_global_env) == VAL_TYPE_SYMBOL) res = 0;

  return res;
}
