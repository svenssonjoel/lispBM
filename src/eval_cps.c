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
#include <stack.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define EVAL_CONTINUE     1
#define PERFORM_GC        2

#define DONE              1
#define SET_GLOBAL_ENV    2
#define FUNCTION_APP      3
#define FUNCTION          4
#define BIND_TO_KEY_REST  5
#define IF                6
#define ARG_LIST          7
#define EVAL              8


static VALUE run_eval(VALUE orig_prg, VALUE lisp, VALUE env);

static VALUE eval_cps_global_env;

stack *K;

static VALUE NIL;

VALUE eval_cps_get_env(void) {
  return eval_cps_global_env;
}

// ////////////////////////////////////////////////////////
// Continuation points and apply cont
// ////////////////////////////////////////////////////////

VALUE cont_set_global_env(VALUE val, bool *perform_gc){

  VALUE curr = eval_cps_global_env;
  VALUE tmp;
  VALUE key;
  pop_u32(K, &key);

  while(type_of(curr) == PTR_TYPE_CONS) {
    if (car(car(curr)) == key) {
      set_cdr(car(curr),val);
      return enc_sym(symrepr_true());
    }
    curr = cdr(curr);
  }
  VALUE keyval;
  keyval = cons(key,val);
  if (type_of(keyval) == VAL_TYPE_SYMBOL) {
    push_u32(K, key);
    push_u32(K, enc_u(SET_GLOBAL_ENV));
    *perform_gc = true;
    return val;
  }

  tmp = cons(keyval, eval_cps_global_env);
  if (type_of(tmp) == VAL_TYPE_SYMBOL) {
    push_u32(K, key);
    push_u32(K, enc_u(SET_GLOBAL_ENV));
    *perform_gc = true;
    return val;
  }

  eval_cps_global_env = tmp;
  return enc_sym(symrepr_true());
}

VALUE apply_continuation(stack *K, VALUE arg, VALUE *curr_exp, VALUE* curr_env, bool *done, bool *perform_gc, bool *app_cont){

  VALUE k;
  pop_u32(K, &k);

  VALUE res;

  *app_cont = false;

  switch(dec_u(k)) {
  case DONE:
    *done = true;
    return arg;
  case EVAL:
    *curr_exp = arg;
    return enc_u(0);
  case SET_GLOBAL_ENV:
    res = cont_set_global_env(arg, perform_gc);
    *app_cont = true;
    return res;

  case FUNCTION_APP: {
    VALUE args;
    VALUE args_rev;
    VALUE fun = arg;

    pop_u32(K, &args);

    if (type_of(args) == PTR_TYPE_CONS) { // TODO: FIX THIS MESS
      args_rev = reverse(args);
      if (type_of(args_rev) == VAL_TYPE_SYMBOL) {
	push_u32(K, args);
	push_u32(K, enc_u(FUNCTION_APP));
	*perform_gc = true;
	*app_cont = true;
	return fun;
      }
    } else {
      args_rev = args;
    }

    if (type_of(fun) == PTR_TYPE_CONS) { // its a closure
       VALUE params  = car(cdr(fun));
       VALUE exp     = car(cdr(cdr(fun)));
       VALUE clo_env = car(cdr(cdr(cdr(fun))));

       VALUE local_env;
       if (length(params) != length(args)) { // programmer error
	 printf("Length mismatch params - args\n");
	 simple_print(params); printf("\n");
	 simple_print(args); printf("\n");
	 *done = true;
	 return enc_sym(symrepr_eerror());
       }

       if (!env_build_params_args(params, args_rev, clo_env, &local_env)) {
	 push_u32(K, args);
	 push_u32(K, enc_u(FUNCTION_APP));
	 *perform_gc = true;
	 *app_cont = true;
	 return fun;
       }
       *curr_exp = exp;
       *curr_env = local_env;
       return 0;

    } else if (type_of(fun) == VAL_TYPE_SYMBOL) { // its a built in function

      VALUE (*f)(VALUE) = builtin_lookup_function(dec_sym(fun));

      if (f == NULL) {
	*done = true;
	return enc_sym(symrepr_eerror());
      }
      VALUE f_res = f(args_rev);

      if (type_of(f_res) == VAL_TYPE_SYMBOL &&
	  (dec_sym(f_res) == symrepr_merror())) {
	push_u32(K, args);
	push_u32(K, enc_u(FUNCTION_APP));
	*perform_gc = true;
	*app_cont = true;
	return fun;
      }
      *app_cont = true;
      return f_res;
    }
  } break;
  case ARG_LIST: {
    VALUE rest;
    VALUE acc;
    VALUE env;
    pop_u32(K, &rest);
    pop_u32(K, &acc);
    pop_u32(K, &env);
    VALUE acc_ = cons(arg, acc);
    if (type_of(acc_) == VAL_TYPE_SYMBOL) {
      push_u32(K, env);
      push_u32(K, acc);
      push_u32(K, rest);
      push_u32(K, enc_u(ARG_LIST));
      *perform_gc = true;
      *app_cont   = true;
      return arg;            // RESET EXECUTION
    }
    if (type_of(rest) == VAL_TYPE_SYMBOL &&
	rest == NIL) {
      *app_cont = true;
      return acc_;
    }
    VALUE head = car(rest);
    push_u32(K, env);
    push_u32(K, acc_);
    push_u32(K, cdr(rest));
    push_u32(K, enc_u(ARG_LIST));
    *curr_env = env;
    *curr_exp = head;
    return enc_u(0);
  }
  case FUNCTION: {
    VALUE fun;
    pop_u32(K, &fun);
    push_u32(K, arg);
    push_u32(K, enc_u(FUNCTION_APP));
    *curr_exp = fun;
    return enc_u(0); // Should return something that is very easy to recognize as nonsense 
  }
  case BIND_TO_KEY_REST:{
    VALUE key;
    VALUE env;
    VALUE rest;
    
    pop_u32(K, &key);
    pop_u32(K, &env);
    pop_u32(K, &rest);
  
    env_modify_binding(env, key, arg);
    
    if ( type_of(rest) == PTR_TYPE_CONS ){
      VALUE keyn = car(car(rest));
      VALUE valn_exp = car(cdr(car(rest)));
      
      push_u32(K, cdr(rest));
      push_u32(K, env);
      push_u32(K, keyn);
      push_u32(K, enc_u(BIND_TO_KEY_REST));

      *curr_exp = valn_exp;
      *curr_env = env;
      return enc_u(0);
    }
    
    // Otherwise evaluate the expression in the populated env
    VALUE exp;
    pop_u32(K, &exp);
    *curr_exp = exp;
    *curr_env = env;
    return enc_u(0);
  }
  case IF: {
    VALUE then_branch;
    VALUE else_branch;

    pop_u32(K, &then_branch);
    pop_u32(K, &else_branch);

    if (type_of(arg) == VAL_TYPE_SYMBOL &&
	dec_sym(arg) == symrepr_true()) {
      *curr_exp = then_branch;
    } else {
      *curr_exp = else_branch;
    }
    return 0;

  } break;
  } // end switch
  *done = true;
  return enc_sym(symrepr_eerror());
}

VALUE run_eval(VALUE orig_prg, VALUE lisp, VALUE env){

  push_u32(K, enc_u(DONE));

  VALUE curr_exp = lisp;
  VALUE curr_env = env;

  VALUE r = NIL;
  bool done = false;
  bool perform_gc = false;
  bool app_cont = false;

  uint32_t non_gc = 0;

  while (!done) {

#ifdef VISUALIZE_HEAP
    heap_vis_gen_image();
#endif

    if (perform_gc) {
      if (non_gc == 0) {
	done = true;
	r = enc_sym(symrepr_eerror());
	continue;
      }
      non_gc = 0;
      heap_perform_gc_aux(eval_cps_global_env, curr_env, curr_exp, orig_prg, K->data, K->size);
      perform_gc = false;
    } else {
      non_gc ++;
    }

    if (app_cont) {
      r = apply_continuation(K, r, &curr_exp, &curr_env, &done, &perform_gc, &app_cont);
      continue;
    }
    app_cont = false;

    VALUE head;
    VALUE value = enc_sym(symrepr_eerror());

    switch (type_of(curr_exp)) {

    case VAL_TYPE_SYMBOL:
      if (!env_lookup(curr_exp, curr_env, &value)) {
	if (!env_lookup(curr_exp, eval_cps_global_env, &value)){
	  r = enc_sym(symrepr_eerror());
	  done = true;
	  continue;
	}
      }
      app_cont = true;
      r = value;
      break;
    case PTR_TYPE_BOXED_F:
    case PTR_TYPE_BOXED_U:
    case PTR_TYPE_BOXED_I:
    case VAL_TYPE_I:
    case VAL_TYPE_U:
    case VAL_TYPE_CHAR:
    case PTR_TYPE_ARRAY:
      app_cont = true;
      r = curr_exp;
      break;
    case PTR_TYPE_REF:
    case PTR_TYPE_STREAM:
      r = enc_sym(symrepr_eerror());
      done = true;
      break;
    case PTR_TYPE_CONS:
      head = car(curr_exp);

      if (type_of(head) == VAL_TYPE_SYMBOL) {

	// Special form: eval
	if (dec_sym(head) == symrepr_eval()) {
	  VALUE exp = car(cdr(curr_exp));
	  push_u32(K, enc_u(EVAL));
	  curr_exp = exp;
	  continue;
	}

	// Special form: QUOTE
	if (dec_sym(head) == symrepr_quote()) {
	  value = car(cdr(curr_exp));
	  app_cont = true;
	  r = value;
	  continue;
	}

	// Special form: DEFINE
	if (dec_sym(head) == symrepr_define()) {
	  VALUE key = car(cdr(curr_exp));
	  VALUE val_exp = car(cdr(cdr(curr_exp)));

	  if (type_of(key) != VAL_TYPE_SYMBOL ||
	      key == NIL) {
	    done = true;
	    r =  enc_sym(symrepr_eerror());
	  }

	  push_u32(K, key);
	  push_u32(K, enc_u(SET_GLOBAL_ENV));
	  curr_exp = val_exp;
	  //curr_env = curr_env;
	  continue;
	}

	// Special form: LAMBDA
	if (dec_sym(head) == symrepr_lambda()) {
	  VALUE env_cpy;

	  if (!env_copy_shallow(curr_env, &env_cpy)) {
	    perform_gc = true;
	    app_cont = false;
	    continue; // perform gc and resume evaluation at same expression
	  }

	  VALUE env_end;
	  VALUE body;
	  VALUE params;
	  VALUE closure;
	  env_end = cons(env_cpy,NIL);
	  body    = cons(car(cdr(cdr(curr_exp))), env_end);
	  params  = cons(car(cdr(curr_exp)), body);
	  closure = cons(enc_sym(symrepr_closure()), params);

	  if (type_of(env_end) == VAL_TYPE_SYMBOL ||
	      type_of(body)    == VAL_TYPE_SYMBOL ||
	      type_of(params)  == VAL_TYPE_SYMBOL ||
	      type_of(closure) == VAL_TYPE_SYMBOL) {
	    perform_gc = true;
	    app_cont = false;
	    continue; // perform gc and resume evaluation at same expression
	  }

	  app_cont = true;
	  r = closure;
	  continue;
	}

	// Special form: IF
	if (dec_sym(head) == symrepr_if()) {

	  push_u32(K, car(cdr(cdr(cdr(curr_exp))))); // else branch
	  push_u32(K, car(cdr(cdr(curr_exp)))); // Then branch
	  push_u32(K, enc_u(IF));
	  curr_exp = car(cdr(curr_exp));
	  continue;
	}
	// Special form: LET
	if (dec_sym(head) == symrepr_let()) {
	  VALUE orig_env = curr_env;
	  VALUE binds    = car(cdr(curr_exp)); // key value pairs.
	  VALUE exp      = car(cdr(cdr(curr_exp))); // exp to evaluate in the new env.

	  VALUE curr = binds;
	  VALUE new_env = orig_env;

	  if (type_of(binds) != PTR_TYPE_CONS) {
	    // binds better be nil or there is a programmer error.
	    curr_exp = exp;
	    continue;
	  }

	  // Implements letrec by "preallocating" the key parts
	  while (type_of(curr) == PTR_TYPE_CONS) {
	    VALUE key = car(car(curr));
	    VALUE val = NIL;
	    VALUE binding;
	    binding = cons(key, val);
	    new_env = cons(binding, new_env);

	    if (type_of(binding) == VAL_TYPE_SYMBOL ||
		type_of(new_env) == VAL_TYPE_SYMBOL) {
	      perform_gc = true;
	      app_cont = false;
	      continue;
	    }
	    curr = cdr(curr);
	  }
	  
	  VALUE key0 = car(car(binds));
	  VALUE val0_exp = car(cdr(car(binds)));
	  
	  push_u32(K, exp);
	  push_u32(K, cdr(binds));
	  push_u32(K, new_env);
	  push_u32(K, key0);
	  push_u32(K, enc_u(BIND_TO_KEY_REST));
	  curr_exp = val0_exp;
	  curr_env = new_env;
	  continue;
	}
      } // If head is symbol
      
      push_u32(K, head);
      push_u32(K, enc_u(FUNCTION));
      if (type_of(cdr(curr_exp)) == VAL_TYPE_SYMBOL &&
	  cdr(curr_exp) == NIL) {
	// no arguments)
	app_cont = true;
	r = NIL;
	continue;
      } else {
	push_u32(K, curr_env);
	push_u32(K, NIL); // accumulator
	push_u32(K, cdr(cdr(curr_exp)));
	push_u32(K, enc_u(ARG_LIST));

	curr_exp = car(cdr(curr_exp));;
	//curr_env = curr_env;
	continue;
      }
    default:
      // BUG No applicable case!
      done = true;
      r = enc_sym(symrepr_eerror());
      break;
    }
  } // while (!done)
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

int eval_cps_init(bool grow_continuation_stack) {
  int res = 1;
  NIL = enc_sym(symrepr_nil());

  eval_cps_global_env = NIL;

  eval_cps_global_env = built_in_gen_env();

  K = stack_init(100, grow_continuation_stack);
  
  VALUE nil_entry = cons(NIL, NIL);
  eval_cps_global_env = cons(nil_entry, eval_cps_global_env);

  if (type_of(nil_entry) == VAL_TYPE_SYMBOL ||
      type_of(eval_cps_global_env) == VAL_TYPE_SYMBOL) res = 0;

  return res;
}

void eval_cps_del(void) {
  stack_del(K);
}
