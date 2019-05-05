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
#define ARG_LIST          9

//#define CONTINUE_EVAL(EXP,ENV) (curr_exp = (EXP), curr_env = (ENV), continue)
//#define GC_ON_ERROR(RES, ALLOCATION) ((RES) = (ALLOCATION), (type_of((RES)) == VAL_TYPE_SYMBOL && dec_sym((RES)) == symrepr_merror()) ? (perform_gc=true, 0) : (RES))
//#define GC_ON_FALSE(STMT) ( (!(STMT)) ? (perform_gc=true,0) : 1)
//#define ABORT_EVAL() (abort=true, continue)

typedef struct {
  VALUE curr_exp;
  VALUE curr_env;

  VALUE K;
  VALUE K_save;
} eval_context_t; 


static VALUE run_eval(VALUE orig_prg, VALUE lisp, VALUE env);
// static VALUE dispatch_continuation(VALUE ix, VALUE args);

jmp_buf rewind_buf;

static VALUE curr_exp;
static VALUE curr_env;
static VALUE eval_cps_global_env;

stack *K;
stack *K_save;

static VALUE NIL;

VALUE eval_cps_get_env(void) {
  return eval_cps_global_env;
}

VALUE eval_cps_bi(VALUE lisp) {
  //CONTINUE_EVAL(car(lisp),curr_env);
}

// ////////////////////////////////////////////////////////
// Continuation points and apply cont
// ////////////////////////////////////////////////////////

VALUE cont_set_global_env(VALUE val, bool *done, bool *perform_gc){

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
    push_u32(&K, key);
    push_u32(&K, enc_u(SET_GLOBAL_ENV));
    //push_u32(&K, val);
    *perform_gc = true;
    return enc_sym(symrepr_merror());
  }

  tmp = cons(keyval, eval_cps_global_env);
  if (type_of(tmp) == VAL_TYPE_SYMBOL) {
    push_u32(&K, key);
    push_u32(&K, enc_u(SET_GLOBAL_ENV));
    //push_u32(&K, val);
    *perform_gc = true;
    return enc_sym(symrepr_merror());
  }

  eval_cps_global_env = tmp;
  return enc_sym(symrepr_true());
}

VALUE apply_continuation(stack *K, VALUE arg, VALUE *curr_exp, VALUE* curr_env, bool *done, bool *perform_gc, bool *app_cont){

  VALUE k;
  pop_u32(K, &k);

  VALUE res;
  
  switch(dec_u(k)) {
  case DONE:
    *done = true;
    return arg;
    
  case SET_GLOBAL_ENV:
    res = cont_set_global_env(arg, done, perform_gc);
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
      *app_cont = true;
      return f_res;
    }
  }
  case ARG_LIST: {
    VALUE rest;
    VALUE acc;
    pop_u32(K, &rest);
    pop_u32(K, &acc);
    VALUE acc_ = cons(arg, acc);
    if (type_of(acc_) == VAL_TYPE_SYMBOL) {
      push_u32(K, acc);
      push_u32(K, rest);
      push_u32(K, enc_u(ARG_LIST));
      *perform_gc = true;
      return arg;            // RESET EXECUTION
    }
    if (type_of(rest) == VAL_TYPE_SYMBOL &&
	dec_sym(rest) == symrepr_nil()) {
      *app_cont = true;
      return acc_;
    } 
    VALUE head = car(rest);  
    push_u32(K, acc_);
    push_u32(K, cdr(rest));
    push_u32(K, enc_u(ARG_LIST));
    *curr_exp = head;
    *app_cont = false;
    return enc_u(777); 
  }
  case FUNCTION: {
    VALUE fun;
    pop_u32(K, &fun);
    push_u32(K, arg);
    push_u32(K, enc_u(FUNCTION_APP));
    *curr_exp = fun;
    *app_cont = false;
    return enc_u(727); // Should return something that is very easy to recognize as nonsense 
  }break;
    /*
  case BIND_TO_KEY_REST:
    return cont_bind_to_key_rest(args);
    break;
  case IF:
    return cont_if(args);
    break;
    */
  }
  
  *done = true; 
  return enc_sym(symrepr_eerror());
  
}

/* 


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


}
*/

VALUE run_eval(VALUE orig_prg, VALUE lisp, VALUE env){

  K = init_stack(100,true);
  K_save = init_stack(100,true);
  
  push_u32(K, enc_u(DONE)); 
  push_u32(K_save, enc_u(DONE));

  curr_exp = lisp;
  curr_env = env;

  VALUE r;
  bool done = false;
  bool perform_gc = false;
  bool app_cont = false; 
  
  while (!done) {

#ifdef VISUALIZE_HEAP
    heap_vis_gen_image();
#endif

    /*
    printf("NEXT ITERATION: "); simple_print(curr_exp); printf("\n");
    printf("PERFORM_GC: %s\n", perform_gc ? "YES" : "NO");
    printf("APP_CONT: %s\n", app_cont ? "YES" : "NO");
    */ 

    if (perform_gc) {
      printf("performing gc\n");
      heap_perform_gc_aux(eval_cps_global_env, curr_env, curr_exp, orig_prg, K->data, K->size);
	//heap_perform_gc_extra(eval_cps_global_env, curr_env, curr_exp, orig_prg, K);
      perform_gc = false;
    }

    if (app_cont) {
      app_cont = false;
      r = apply_continuation(K, r, &curr_exp, &curr_env, &done, &perform_gc, &app_cont);
      continue;
    }

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
	  curr_env = env;
	  continue;
	}
	
	// Special form: LAMBDA
	if (dec_sym(head) == symrepr_lambda()) {
	  VALUE env_cpy;
	  
	  if (!env_copy_shallow(env,&env_cpy)) {
	    printf("GC NEEDED\n");
	    perform_gc = true;
	    continue;
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
	    printf("GC NEEDED\n");
	    perform_gc = true;
	    continue;
	  }

	  app_cont = true; 
	  r = closure;
	  continue;
	}
      }
	/*
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
	*/
      // Possibly a function application form:

      push_u32(K, head);
      push_u32(K, enc_u(FUNCTION));

      if (type_of(cdr(curr_exp)) == VAL_TYPE_SYMBOL &&
	  dec_sym(cdr(curr_exp)) == symrepr_nil()) {
	// no arguments)
	app_cont = true;
	r = enc_sym(symrepr_nil);
	continue; 
      } else { 
	push_u32(K, enc_sym(symrepr_nil())); // accumulator
	push_u32(K, cdr(cdr(curr_exp)));
	push_u32(K, enc_u(ARG_LIST));
	
	curr_exp = car(cdr(curr_exp));;
	curr_env = curr_env;
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
