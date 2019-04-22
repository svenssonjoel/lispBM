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
//#include "stack.h"

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
#define GC_ON_FALSE(STMT) ( (!(STMT)) ? (longjmp(rewind_buf, PERFORM_GC),0) : 1)

static VALUE run_eval(VALUE orig_prg, VALUE lisp, VALUE env);
static VALUE dispatch_continuation(VALUE ix, VALUE args);

jmp_buf rewind_buf;

// Removed volatile qualifier on these.
static VALUE curr_exp;
static VALUE curr_env;
static VALUE eval_cps_global_env;

static VALUE NIL;

//stack *K; // Stack describes the current continuation.
//stack *K_save; // Stack save area for resume after gc.

VALUE K;
VALUE K_save;

VALUE eval_cps_get_env(void) {
  return eval_cps_global_env;
}

VALUE eval_cps_bi(VALUE lisp) {
  CONTINUE_EVAL(car(lisp),curr_env);
}

// ////////////////////////////////////////////////////////
// Continuation points and apply cont
// ////////////////////////////////////////////////////////


int push(VALUE val) {
  
  VALUE cons_cell = cons(val, K);
  if (type_of(cons_cell) == VAL_TYPE_SYMBOL &&
      dec_sym(cons_cell) == symrepr_merror()) {
    return 0; 
  }
  K = cons_cell;
  return 1;
}

VALUE pop() {
  VALUE val;
  val = car(K);
  K = cdr(K);
  return val;
}


VALUE apply_continuation(){

  VALUE k;
  VALUE args;
  args = pop();
  k = pop();
  //pop_u32(K, &args);
  //pop_u32(K, &k);
  return dispatch_continuation(k, args);
}

VALUE cont_done(VALUE arg) {
  return arg;
}

VALUE cont_set_global_env(VALUE val){

  VALUE curr = eval_cps_global_env;
  VALUE tmp;
  VALUE key;
  //pop_u32(K,&key);
  key = pop();
  
  while(type_of(curr) == PTR_TYPE_CONS) {
    if (car(car(curr)) == key) {
      set_cdr(car(curr),val);

      return enc_sym(symrepr_true());
    }
    curr = cdr(curr);
  }
  VALUE keyval;
  GC_ON_ERROR(keyval, cons(key,val));

  GC_ON_ERROR(tmp, cons(keyval, eval_cps_global_env));

  eval_cps_global_env = tmp;
  return enc_sym(symrepr_true());
}


VALUE cont_function_app(VALUE args) {

  VALUE args_rev;
  VALUE fun;
  //pop_u32(K, &fun);
  fun = pop(); 
  
  if (type_of(args) == PTR_TYPE_CONS) { // TODO: FIX THIS MESS
    GC_ON_ERROR(args_rev, reverse(args));
  } else {
    args_rev = args;
  }

  VALUE (*f)(VALUE) = builtin_lookup_function(dec_sym(fun));

  if (f == NULL) {
    return enc_sym(symrepr_eerror());
  }
  VALUE f_res = f(args_rev);
  //push_u32(K, f_res);
  GC_ON_FALSE(push(f_res));
  return apply_continuation();
}

VALUE cont_closure_app(VALUE args) {

  VALUE args_rev;
  VALUE closure;
  //pop_u32(K, &closure);
  closure = pop();
  
  if (type_of(args) == PTR_TYPE_CONS) { // TODO: FIX THIS MESS
    GC_ON_ERROR(args_rev, reverse(args));
  } else {
    args_rev = args;
  }

  VALUE params  = car(cdr(closure));
  VALUE exp     = car(cdr(cdr(closure)));
  VALUE clo_env = car(cdr(cdr(cdr(closure))));

  // TODO: env_build_params_args should follow the same interface as the rest.
  //       Return symbol error on failure.
  VALUE local_env;
  if (!env_build_params_args(params, args_rev, clo_env, &local_env)) {
    longjmp(rewind_buf, PERFORM_GC);
  }

  CONTINUE_EVAL(exp,local_env);
}

VALUE cont_eval_rest(VALUE head) {
  
  VALUE rest;
  VALUE acc;
  VALUE env;

  //pop_u32(K, &rest);
  //pop_u32(K, &acc);
  //pop_u32(K, &env);
  rest = pop();
  acc = pop();
  env = pop();

  if (type_of(rest) == VAL_TYPE_SYMBOL &&
      rest == NIL) {

    VALUE args;
    GC_ON_ERROR(args, cons(head,acc));

    //push_u32(K, args);
    GC_ON_FALSE(push(args));
    return apply_continuation();
  }

  GC_ON_ERROR(acc, cons(head, acc));

  //push_u32(K, env);
  //push_u32(K, acc);
  //push_u32(K, cdr(rest));
  //push_u32(K, enc_u(EVAL_REST));
  GC_ON_FALSE(push(env));
  GC_ON_FALSE(push(acc));
  GC_ON_FALSE(push(cdr(rest)));
  GC_ON_FALSE(push(enc_u(EVAL_REST)));
    
  
  CONTINUE_EVAL(car(rest),env);
}

// Closure or built-in function
VALUE cont_function(VALUE fun) {

  VALUE fun_args;
  VALUE env;
  //pop_u32(K,&fun_args);
  //pop_u32(K,&env);
  fun_args = pop();
  env = pop();
  
  VALUE head = car(fun_args);

  //push_u32(K,fun);
  GC_ON_FALSE(push(fun));
  if ( type_of(fun) == PTR_TYPE_CONS &&
       dec_sym(car(fun)) == symrepr_closure()) {
    //push_u32(K, enc_u(CLOSURE_APP));
    GC_ON_FALSE(push(enc_u(CLOSURE_APP)));
  } else {
    //push_u32(K, enc_u(FUNCTION_APP));
    GC_ON_FALSE(push(enc_u(FUNCTION_APP)));
  }
  // If args are a list with at least one element, process the elements
  if (type_of(fun_args) == PTR_TYPE_CONS &&
      length(fun_args) >= 1) {
    // Check if this really makes sense. 
    //push_u32(K,env);
    //push_u32(K,NIL);
    //push_u32(K,cdr(fun_args));
    //push_u32(K, enc_u(EVAL_REST));
    GC_ON_FALSE(push(env));
    GC_ON_FALSE(push(NIL));
    GC_ON_FALSE(push(cdr(fun_args)));
    GC_ON_FALSE(push(enc_u(EVAL_REST)));

    CONTINUE_EVAL(head,env);
  }
  // otherwise the arguments are an empty list (or something bad happened)
  //push_u32(K, NIL);
  GC_ON_FALSE(push(NIL));
  return apply_continuation();
}


VALUE cont_bind_to_key_rest(VALUE val) {
  VALUE key;
  VALUE env;
  VALUE rest;

  //pop_u32(K, &key);
  //pop_u32(K, &env);
  //pop_u32(K, &rest);
  key = pop();
  env = pop();
  rest = pop(); 

  env_modify_binding(env, key, val);

  if ( type_of(rest) == PTR_TYPE_CONS ){
    VALUE keyn = car(car(rest));
    VALUE valn_exp = car(cdr(car(rest)));

    //push_u32(K,cdr(rest));
    //push_u32(K,env);
    //push_u32(K,keyn);
    //push_u32(K, enc_u(BIND_TO_KEY_REST));
    GC_ON_FALSE(push(cdr(rest)));
    GC_ON_FALSE(push(env));
    GC_ON_FALSE(push(keyn));
    GC_ON_FALSE(push(enc_u(BIND_TO_KEY_REST)));
    
    CONTINUE_EVAL(valn_exp,env);
  }

  // Otherwise evaluate the expression in the populated env
  VALUE exp;
  //pop_u32(K, &exp);
  exp = pop(); 

  CONTINUE_EVAL(exp,env);
}

VALUE cont_if(VALUE cond) {

  VALUE then_branch;
  VALUE else_branch;

  //pop_u32(K, &then_branch);
  //pop_u32(K, &else_branch);
  then_branch = pop();
  else_branch = pop(); 

  if (type_of(cond) == VAL_TYPE_SYMBOL &&
      dec_sym(cond) == symrepr_true()) {
    CONTINUE_EVAL(then_branch,curr_env);
  } else {
    CONTINUE_EVAL(else_branch,curr_env);
  }
}

VALUE dispatch_continuation(VALUE ix, VALUE args) {

  switch(dec_u(ix)) {
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

VALUE process_let(VALUE binds, VALUE orig_env, VALUE exp) {
  VALUE curr = binds;
  VALUE new_env = orig_env;

  if (type_of(binds) != PTR_TYPE_CONS) {
    return enc_sym(symrepr_eerror());
  }

  while (type_of(curr) == PTR_TYPE_CONS) {
    VALUE key = car(car(curr));
    VALUE val = NIL; // a temporary
    VALUE binding;
    GC_ON_ERROR(binding, cons(key,val));

    GC_ON_ERROR(new_env, cons(binding, new_env));

    curr = cdr(curr);
  }

  VALUE key0 = car(car(binds));
  VALUE val0_exp = car(cdr(car(binds)));

  //push_u32(K,exp); // exp to evaluate in new environment
  //push_u32(K,cdr(binds));
  //push_u32(K,new_env);
  //push_u32(K,key0);
  //push_u32(K, enc_u(BIND_TO_KEY_REST));
  GC_ON_FALSE(push(exp));
  GC_ON_FALSE(push(cdr(binds)));
  GC_ON_FALSE(push(new_env));
  GC_ON_FALSE(push(key0));
  GC_ON_FALSE(push(enc_u(BIND_TO_KEY_REST)));
  
  CONTINUE_EVAL(val0_exp,new_env);
}

// ////////////////////////////////////////////////////////
// EVALUATION
// ////////////////////////////////////////////////////////
VALUE eval_cps(VALUE lisp, VALUE env) {

  VALUE head;
  VALUE value = enc_sym(symrepr_eerror());

  switch (type_of(lisp)) {

  case VAL_TYPE_SYMBOL:
    if (!env_lookup(lisp, env, &value)) {
      if (!env_lookup(lisp, eval_cps_global_env, &value)){
	return enc_sym(symrepr_eerror());
      }
    }
    //push_u32(K, value);
    GC_ON_FALSE(push(value));
    return apply_continuation();

  case PTR_TYPE_BOXED_F:
  case PTR_TYPE_BOXED_U:
  case PTR_TYPE_BOXED_I:
  case VAL_TYPE_I:
  case VAL_TYPE_U:
  case VAL_TYPE_CHAR:
  case PTR_TYPE_ARRAY:
    //push_u32(K, lisp);
    GC_ON_FALSE(push(lisp));
    return apply_continuation();

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
	//push_u32(K, value);
	GC_ON_FALSE(push(value));
	return apply_continuation();
      }

      // Special form: DEFINE
      if (dec_sym(head) == symrepr_define()) {
	VALUE key = car(cdr(lisp));
	VALUE val_exp = car(cdr(cdr(lisp)));

	if (type_of(key) != VAL_TYPE_SYMBOL ||
	    key == NIL) {
	  return enc_sym(symrepr_eerror());
	}

	//push_u32(K, key);
	//push_u32(K, enc_u(SET_GLOBAL_ENV));
	GC_ON_FALSE(push(key));
	GC_ON_FALSE(push(enc_u(SET_GLOBAL_ENV)));

	CONTINUE_EVAL(val_exp,env);
      }

      // Special form: LAMBDA
      if (dec_sym(head) == symrepr_lambda()) {
	VALUE env_cpy;

	GC_ON_FALSE(env_copy_shallow(env,&env_cpy));
	
	VALUE env_end = cons(env_cpy,NIL);
	VALUE body    = cons(car(cdr(cdr(lisp))), env_end);
	VALUE params  = cons(car(cdr(lisp)), body);
	VALUE closure = cons(enc_sym(symrepr_closure()), params);

	if (type_of(env_end) == VAL_TYPE_SYMBOL ||
	    type_of(body)    == VAL_TYPE_SYMBOL ||
	    type_of(params)  == VAL_TYPE_SYMBOL ||
	    type_of(closure) == VAL_TYPE_SYMBOL) {
	  longjmp(rewind_buf, PERFORM_GC);
	}

	//push_u32(K,closure);
	GC_ON_FALSE(push(closure));
	return apply_continuation();
      }

      // Special form: IF
      if (dec_sym(head) == symrepr_if()) {

	//push_u32(K,car(cdr(cdr(cdr(lisp))))); // else branch
	//push_u32(K,car(cdr(cdr(lisp)))); // Then branch
	//push_u32(K, enc_u(IF));
	
	GC_ON_FALSE(push(car(cdr(cdr(cdr(lisp)))))); // else branch
	GC_ON_FALSE(push(car(cdr(cdr(lisp))))); // Then branch
	GC_ON_FALSE(push(enc_u(IF)));
	
	CONTINUE_EVAL(car(cdr(lisp)),curr_env);
      }

      // Special form: LET
      if (dec_sym(head) == symrepr_let()) {
	VALUE orig_env = env;
	VALUE binds   = car(cdr(lisp)); // key value pairs.
	VALUE exp     = car(cdr(cdr(lisp))); // exp to evaluate in the new env.
	// Setup the bindings
	return process_let(binds, orig_env, exp);
      }
    } // If head is symbol

    // Possibly a function application form:
    //push_u32(K, curr_env);  // The environment each element should be evaluated in 
    //push_u32(K, cdr(lisp)); // list of arguments that needs to be evaluated.
    //push_u32(K, enc_u(FUNCTION));
    GC_ON_FALSE(push(curr_env));
    GC_ON_FALSE(push(cdr(lisp)));
    GC_ON_FALSE(push(enc_u(FUNCTION)));

    
    CONTINUE_EVAL(head,curr_env);

  default:
    // BUG No applicable case!
    return enc_sym(symrepr_eerror());
    break;
  }
}


VALUE run_eval(VALUE orig_prg, VALUE lisp, VALUE env){

  //push_u32(K, enc_u(DONE));
  //push_u32(K_save, enc_u(DONE));
  GC_ON_FALSE(push(enc_u(DONE)));
  K_save = copy(K);
  
  curr_exp = lisp;
  curr_env = env;

  VALUE r;

  int res = setjmp(rewind_buf);

  if (res) {

#ifdef VISUALIZE_HEAP
    heap_vis_gen_image();
#endif
    if (res == PERFORM_GC) {
      K = K_save; 

      heap_perform_gc_extra(eval_cps_global_env, curr_env, curr_exp, orig_prg, K);
      //heap_perform_gc_aux(eval_cps_global_env, curr_env, curr_exp, orig_prg, K->data, K->sp);
    }

    // Copying the K stack on the heap here may result in the need for a GC...
    // what to do? 
    K_save = copy(K);
    if (type_of(K_save) == VAL_TYPE_SYMBOL &&
	dec_sym(K_save) == symrepr_merror()) {
      heap_perform_gc_extra(eval_cps_global_env, curr_env, curr_exp, orig_prg, K);
      K_save = copy(K);
      if (type_of(K_save) == VAL_TYPE_SYMBOL &&
	  dec_sym(K_save) == symrepr_merror()) {
	printf("ERROR: Not enough memory to proceed\n");
	return 0;
      }
    }
    
    //copy_stack(K_save, K);

    r = eval_cps(curr_exp, curr_env);

  } else {

    // kickstarts evaluation with the done_cont;
    r = eval_cps(curr_exp, curr_env);
  }

  if (type_of(r) == VAL_TYPE_SYMBOL &&
      (dec_sym(r) == symrepr_eerror() ||
       dec_sym(r) == symrepr_merror() ||
       dec_sym(r) == symrepr_terror())) {
    //clear_stack(K);
    //clear_stack(K_save);
    K = NIL;
    K_save = NIL;
  }

  return r;
}

VALUE eval_cps_program(VALUE lisp) {

  VALUE res = NIL;
  VALUE local_env = NIL;
  VALUE curr = lisp;

  while (type_of(curr) == PTR_TYPE_CONS) {
    res =  run_eval(lisp, car(curr),local_env);
    curr = cdr(curr);
  }
  return res;
}



int eval_cps_init() {

  //K = init_cont_stack(1000);
  //K_save = init_cont_stack(1000);
  
  int res = builtin_add_function("eval",eval_cps_bi);

  NIL = enc_sym(symrepr_nil());

  K = NIL;
  K_save = NIL;
  
  eval_cps_global_env = NIL;

  eval_cps_global_env = built_in_gen_env();

  VALUE nil_entry = cons(NIL, NIL);
  eval_cps_global_env = cons(nil_entry, eval_cps_global_env);

  if (type_of(nil_entry) == VAL_TYPE_SYMBOL ||
      type_of(eval_cps_global_env) == VAL_TYPE_SYMBOL) res = 0;

  return res;
}
