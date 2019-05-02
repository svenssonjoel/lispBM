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
#define ABORT             3

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
#define ABORT_EVAL() longjmp(rewind_buf, PERFORM_GC)

static VALUE run_eval(VALUE orig_prg, VALUE lisp, VALUE env);
static VALUE dispatch_continuation(VALUE ix, VALUE args);

jmp_buf rewind_buf;

static VALUE curr_exp;
static VALUE curr_env;
static VALUE eval_cps_global_env;

static VALUE NIL;

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


int push(VALUE *K, VALUE val) {

  VALUE cons_cell = cons(val, *K);
  if (type_of(cons_cell) == VAL_TYPE_SYMBOL &&
      dec_sym(cons_cell) == symrepr_merror()) {
    return 0;
  }
  *K = cons_cell;
  return 1;
}

VALUE pop(VALUE *K) {
  VALUE val;
  val = car(*K);
  *K = cdr(*K);
  return val;
}


VALUE apply_continuation(VALUE *K){

  VALUE k;
  VALUE args;
  args = pop(K);
  k = pop(K);
  return dispatch_continuation(k, args);
}

VALUE cont_done(VALUE arg) {
  return arg;
}

VALUE cont_set_global_env(VALUE val){

  VALUE curr = eval_cps_global_env;
  VALUE tmp;
  VALUE key;
  key = pop(&K);

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
  fun = pop(&K);

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
  GC_ON_FALSE(push(&K, f_res));
  return apply_continuation(&K);
}

VALUE cont_closure_app(VALUE args) {

  VALUE args_rev;
  VALUE closure;
  closure = pop(&K);

  if (type_of(args) == PTR_TYPE_CONS) { // TODO: FIX THIS MESS
    GC_ON_ERROR(args_rev, reverse(args));
  } else {
    args_rev = args;
  }

  VALUE params  = car(cdr(closure));
  VALUE exp     = car(cdr(cdr(closure)));
  VALUE clo_env = car(cdr(cdr(cdr(closure))));

  VALUE local_env;
  if (length(params) != length(args)) { // programmer error
    printf("Length mismatch params - args\n");
    simple_print(params); printf("\n");
    simple_print(args); printf("\n");
    ABORT_EVAL();
  }

  GC_ON_FALSE(env_build_params_args(params, args_rev, clo_env, &local_env));

  CONTINUE_EVAL(exp,local_env);
}

VALUE cont_eval_rest(VALUE head) {

  VALUE rest;
  VALUE acc;
  VALUE env;

  rest = pop(&K);
  acc = pop(&K);
  env = pop(&K);

  if (type_of(rest) == VAL_TYPE_SYMBOL &&
      rest == NIL) {

    VALUE args;
    GC_ON_ERROR(args, cons(head,acc));

    GC_ON_FALSE(push(&K, args));
    return apply_continuation(&K);
  }

  GC_ON_ERROR(acc, cons(head, acc));
  GC_ON_FALSE(push(&K, env));
  GC_ON_FALSE(push(&K, acc));
  GC_ON_FALSE(push(&K, cdr(rest)));
  GC_ON_FALSE(push(&K, enc_u(EVAL_REST)));


  CONTINUE_EVAL(car(rest),env);
}

// Closure or built-in function
VALUE cont_function(VALUE fun) {

  VALUE fun_args;
  VALUE env;
  fun_args = pop(&K);
  env = pop(&K);

  VALUE head = car(fun_args);

  GC_ON_FALSE(push(&K, fun));
  if ( type_of(fun) == PTR_TYPE_CONS &&
       dec_sym(car(fun)) == symrepr_closure()) {
    GC_ON_FALSE(push(&K, enc_u(CLOSURE_APP)));
  } else {
    GC_ON_FALSE(push(&K, enc_u(FUNCTION_APP)));
  }
  // If args are a list with at least one element, process the elements
  if (type_of(fun_args) == PTR_TYPE_CONS &&
      length(fun_args) >= 1) {
    GC_ON_FALSE(push(&K, env));
    GC_ON_FALSE(push(&K, NIL));
    GC_ON_FALSE(push(&K, cdr(fun_args)));
    GC_ON_FALSE(push(&K, enc_u(EVAL_REST)));

    CONTINUE_EVAL(head,env);
  }
  // otherwise the arguments are an empty list (or something bad happened)
  GC_ON_FALSE(push(&K, NIL));
  return apply_continuation(&K);
}


VALUE cont_bind_to_key_rest(VALUE val) {
  VALUE key;
  VALUE env;
  VALUE rest;

  key = pop(&K);
  env = pop(&K);
  rest = pop(&K);

  // TODO: Look for bug in environment handling
  //       that shows up in some cases when having lets in lambdas. 
  env_modify_binding(env, key, val);

  if ( type_of(rest) == PTR_TYPE_CONS ){
    VALUE keyn = car(car(rest));
    VALUE valn_exp = car(cdr(car(rest)));

    GC_ON_FALSE(push(&K, cdr(rest)));
    GC_ON_FALSE(push(&K, env));
    GC_ON_FALSE(push(&K, keyn));
    GC_ON_FALSE(push(&K, enc_u(BIND_TO_KEY_REST)));

    CONTINUE_EVAL(valn_exp,env);
  }

  // Otherwise evaluate the expression in the populated env
  VALUE exp;
  exp = pop(&K);

  CONTINUE_EVAL(exp,env);
}

VALUE cont_if(VALUE cond) {

  VALUE then_branch;
  VALUE else_branch;

  then_branch = pop(&K);
  else_branch = pop(&K);

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

  GC_ON_FALSE(push(&K, exp));
  GC_ON_FALSE(push(&K, cdr(binds)));
  GC_ON_FALSE(push(&K, new_env));
  GC_ON_FALSE(push(&K, key0));
  GC_ON_FALSE(push(&K, enc_u(BIND_TO_KEY_REST)));

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
    GC_ON_FALSE(push(&K, value));
    return apply_continuation(&K);

  case PTR_TYPE_BOXED_F:
  case PTR_TYPE_BOXED_U:
  case PTR_TYPE_BOXED_I:
  case VAL_TYPE_I:
  case VAL_TYPE_U:
  case VAL_TYPE_CHAR:
  case PTR_TYPE_ARRAY:
    GC_ON_FALSE(push(&K, lisp));
    return apply_continuation(&K);

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
	GC_ON_FALSE(push(&K, value));
	return apply_continuation(&K);
      }

      // Special form: DEFINE
      if (dec_sym(head) == symrepr_define()) {
	VALUE key = car(cdr(lisp));
	VALUE val_exp = car(cdr(cdr(lisp)));

	if (type_of(key) != VAL_TYPE_SYMBOL ||
	    key == NIL) {
	  return enc_sym(symrepr_eerror());
	}

	GC_ON_FALSE(push(&K, key));
	GC_ON_FALSE(push(&K, enc_u(SET_GLOBAL_ENV)));

	CONTINUE_EVAL(val_exp,env);
      }

      // Special form: LAMBDA
      if (dec_sym(head) == symrepr_lambda()) {
	VALUE env_cpy;

	GC_ON_FALSE(env_copy_shallow(env,&env_cpy));

	VALUE env_end = GC_ON_ERROR(env_end, cons(env_cpy,NIL));
	VALUE body    = GC_ON_ERROR(body, cons(car(cdr(cdr(lisp))), env_end));
	VALUE params  = GC_ON_ERROR(params, cons(car(cdr(lisp)), body));
	VALUE closure = GC_ON_ERROR(closure, cons(enc_sym(symrepr_closure()), params));

	GC_ON_FALSE(push(&K, closure));
	return apply_continuation(&K);
      }

      // Special form: IF
      if (dec_sym(head) == symrepr_if()) {

	GC_ON_FALSE(push(&K, car(cdr(cdr(cdr(lisp)))))); // else branch
	GC_ON_FALSE(push(&K, car(cdr(cdr(lisp))))); // Then branch
	GC_ON_FALSE(push(&K, enc_u(IF)));

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
    GC_ON_FALSE(push(&K, curr_env));
    GC_ON_FALSE(push(&K, cdr(lisp)));
    GC_ON_FALSE(push(&K, enc_u(FUNCTION)));

    CONTINUE_EVAL(head,curr_env);

  default:
    // BUG No applicable case!
    return enc_sym(symrepr_eerror());
    break;
  }
}


VALUE run_eval(VALUE orig_prg, VALUE lisp, VALUE env){

  push(&K, enc_u(DONE)); // Check for error (Abort on error)
  K_save = copy(K);

  curr_exp = lisp;
  curr_env = env;

  VALUE r;

  int res = setjmp(rewind_buf);

  if (res) {

    if (res == ABORT) {
      return (enc_sym(symrepr_eerror()));
    }
#ifdef VISUALIZE_HEAP
    heap_vis_gen_image();
#endif
    if (res == PERFORM_GC) {
      K = K_save;

      heap_perform_gc_extra(eval_cps_global_env, curr_env, curr_exp, orig_prg, K);
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

    r = eval_cps(curr_exp, curr_env);

  } else {

    // kickstarts evaluation with the done_cont;
    r = eval_cps(curr_exp, curr_env);
  }

  if (type_of(r) == VAL_TYPE_SYMBOL &&
      (dec_sym(r) == symrepr_eerror() ||
       dec_sym(r) == symrepr_merror() ||
       dec_sym(r) == symrepr_terror())) {
    K = NIL;
    K_save = NIL;
  }

  return r;
}

VALUE eval_cps_program(VALUE lisp) {

  VALUE res = NIL;
  VALUE local_env = NIL;
  VALUE curr = lisp;

  if (dec_sym(lisp) == symrepr_eerror() ||
      dec_sym(lisp) == symrepr_rerror() ||
      dec_sym(lisp) == symrepr_merror() ||
      dec_sym(lisp) == symrepr_terror())  return lisp;

  while (type_of(curr) == PTR_TYPE_CONS) {
    res =  run_eval(lisp, car(curr),local_env);
    curr = cdr(curr);
  }
  return res;
}



int eval_cps_init() {

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
