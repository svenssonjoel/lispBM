/*
    Copyright 2020      Joel Svensson	svenssonjoel@yahoo.se

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
#include "env.h"
#include "stack.h"
#include "fundamental.h"
#include "extensions.h"
#include "typedefs.h"
#include "ec_eval.h"

typedef enum {
  EXP_KIND_ERROR,
  EXP_SELF_EVALUATING,
  EXP_VARIABLE,
  EXP_QUOTED,
  EXP_DEFINE,
  EXP_LAMBDA,
  EXP_IF,
  EXP_NO_ARGS,
  EXP_APPLICATION,
  EXP_LET
} exp_kind;

typedef enum {
  CONT_DONE, 
  CONT_DEFINE,
  CONT_SETUP_NO_ARG_APPLY,
  CONT_EVAL_ARGS,
  CONT_ACCUMULATE_ARG,
  CONT_ACCUMULATE_LAST_ARG
} continuation;

typedef enum {
  EVAL_DISPATCH,
  EVAL_CONTINUATION,
  EVAL_ARG_LOOP,
  EVAL_LAST_ARG,
  EVAL_APPLY_DISPATCH
} eval_state;

/*
   Machine model:
   7 x 32 Bit Registers
   + Stack
*/
typedef struct {
  uint32_t cont;
  VALUE env;
  VALUE unev;
  VALUE exp;
  VALUE argl;
  VALUE val;
  VALUE fun;

  stack S;
} register_machine_t;

register_machine_t rm_state;
VALUE ec_eval_global_env;

exp_kind kind_of(VALUE exp) {

  switch (type_of(exp)) {
  case VAL_TYPE_SYMBOL:
    if (!is_special(exp))
      return EXP_VARIABLE;
    // fall through
  case PTR_TYPE_BOXED_F:
  case PTR_TYPE_BOXED_U:
  case PTR_TYPE_BOXED_I:
  case VAL_TYPE_I:
  case VAL_TYPE_U:
  case VAL_TYPE_CHAR:
  case PTR_TYPE_ARRAY:
    return EXP_SELF_EVALUATING;
  case PTR_TYPE_CONS: {
    VALUE head = car(rm_state.exp);
    if (type_of(head) == VAL_TYPE_SYMBOL) {
      UINT sym_id = dec_sym(head);

      if (sym_id == symrepr_quote())
	return EXP_QUOTED;
      if (sym_id == symrepr_define())
	return EXP_DEFINE;
      if (sym_id == symrepr_progn())
	return EXP_APPLICATION;
      if (sym_id == symrepr_lambda())
	return EXP_LAMBDA;
      if (sym_id == symrepr_if())
	return EXP_IF;
      if (sym_id == symrepr_let())
	return EXP_LET;
      if (type_of(cdr(exp)) == VAL_TYPE_SYMBOL &&
	  dec_sym(cdr(exp)) == symrepr_nil()) {
	return EXP_NO_ARGS;
      }
      return EXP_APPLICATION;
    } // end if symbol
  } // end case PTR_TYPE_CONS:
  }
  return EXP_KIND_ERROR;
}

static inline bool last_operand(VALUE exp) {
  if (type_of(cdr(exp)) == VAL_TYPE_SYMBOL &&
      dec_sym(cdr(exp)) == symrepr_nil())
    return true;
  return false; 
}

static inline void eval_self_evaluating(eval_state *es) {
  rm_state.val = rm_state.exp;
  *es = EVAL_CONTINUATION;
}

static inline void eval_variable(eval_state *es) {
  rm_state.val = env_lookup(rm_state.exp, rm_state.env);
  *es = EVAL_CONTINUATION;
}

static inline void eval_quoted(eval_state *es) {
  rm_state.val = car(cdr(rm_state.exp));
  *es = EVAL_CONTINUATION;
}

static inline void eval_define(eval_state *es) {
  rm_state.unev = car(cdr(rm_state.exp));
  rm_state.exp  = car(cdr(cdr(rm_state.exp)));
  push_u32_3(&rm_state.S,
	     rm_state.unev,
	     rm_state.env,
	     rm_state.cont);
  rm_state.cont = CONT_DEFINE;
  *es = EVAL_DISPATCH;
}

static inline void cont_define(eval_state *es) {
  pop_u32_3(&rm_state.S,
	    &rm_state.cont,
	    &rm_state.env,
	    &rm_state.unev);
  VALUE new_env = env_set(ec_eval_global_env,
			  rm_state.unev,
			  rm_state.val);
  ec_eval_global_env = new_env;

  // TODO: error checking and garbage collection
  rm_state.val = rm_state.unev;
  *es = EVAL_CONTINUATION;
}

static inline VALUE mkClosure(void) {

  VALUE env_end = cons(rm_state.env, enc_sym(symrepr_nil()));
  VALUE body    = cons(car(cdr(cdr(rm_state.exp))), env_end);
  VALUE params  = cons(car(cdr(rm_state.exp)),body);
  VALUE closure = cons(enc_sym(symrepr_closure()), params);

  //TODO: error checking and garbage collection
  return closure;
}

static inline void eval_lambda(eval_state *es) {
  rm_state.val = mkClosure();
  *es = EVAL_CONTINUATION;
}

static inline void eval_no_args(eval_state *es) {
  rm_state.exp = car(rm_state.exp);
  push_u32(&rm_state.S, rm_state.cont);
  rm_state.cont = CONT_SETUP_NO_ARG_APPLY;
  *es = EVAL_DISPATCH;
}

static inline void cont_setup_no_arg_apply(eval_state *es) {
  rm_state.fun = rm_state.val;
  rm_state.argl = enc_sym(symrepr_nil());
  *es = EVAL_APPLY_DISPATCH;
}

static inline void eval_application(eval_state *es) {
  rm_state.unev = cdr(rm_state.exp);
  rm_state.exp = car(rm_state.exp);
  push_u32_3(&rm_state.S, rm_state.cont, rm_state.env, rm_state.unev);
  rm_state.cont = CONT_EVAL_ARGS;
  *es = EVAL_DISPATCH;
}

static inline void cont_eval_args(eval_state *es) {
  pop_u32_2(&rm_state.S,&rm_state.unev, &rm_state.env);
  rm_state.fun = rm_state.val;
  push_u32(&rm_state.S,rm_state.fun);
  rm_state.argl = enc_sym(symrepr_nil());
  *es = EVAL_ARG_LOOP;
}

static inline void eval_arg_loop(eval_state *es) {
  push_u32(&rm_state.S, rm_state.argl);
  rm_state.exp = car(rm_state.unev);
  if (last_operand(rm_state.unev)) {
    *es = EVAL_LAST_ARG;
    return;
  }
  push_u32_2(&rm_state.S, rm_state.env, rm_state.unev);
  rm_state.cont = CONT_ACCUMULATE_ARG;
  *es = EVAL_DISPATCH;
}

static inline void cont_accumulate_arg(eval_state *es) {
  pop_u32_3(&rm_state.S, &rm_state.unev, &rm_state.env, &rm_state.argl);
  rm_state.argl = cons(rm_state.val, rm_state.argl);  // TODO error checking and garbage collection
  rm_state.unev = cdr(rm_state.unev);
  *es = EVAL_ARG_LOOP;
}

static inline void eval_last_arg(eval_state *es) {
  rm_state.cont = CONT_ACCUMULATE_LAST_ARG;
  *es = EVAL_DISPATCH;
}

static inline void cont_accumulate_last_arg(eval_state *es) {
  pop_u32(&rm_state.S, &rm_state.argl);
  rm_state.argl = cons(rm_state.val, rm_state.argl); // TODO error checking and garbage collection
  pop_u32(&rm_state.S, &rm_state.fun);
  *es = EVAL_DISPATCH;
}

static inline void eval_apply_fundamental(eval_state *es) {
  UINT count = 0;
  VALUE args = rm_state.argl;
  while (type_of(args) == PTR_TYPE_CONS) {
    push_u32(&rm_state.S, car(args));
    count ++;
  }
  UINT *fun_args = stack_ptr(&rm_state.S, dec_u(count));
  rm_state.val = fundamental_exec(fun_args, count, rm_state.fun);
  stack_drop(&rm_state.S, count);
  *es = EVAL_CONTINUATION;
}

static inline void eval_apply_closure(eval_state *es) {
  VALUE local_env = env_build_params_args(car(cdr(rm_state.fun)),
					  rm_state.argl,
					  car(cdr(cdr(cdr(rm_state.fun)))));
  //TODO: Error checking and garbage collection
  rm_state.env = local_env;
  rm_state.exp = car(cdr(cdr(rm_state.fun)));
  *es = EVAL_DISPATCH;			      
}

static inline void eval_apply_extension(eval_state *es) {
  (void) es;
}

static inline void eval_apply_dispatch(eval_state *es) {
  if (is_fundamental(rm_state.fun)) eval_apply_fundamental(es);
  else if (is_closure(rm_state.fun)) eval_apply_closure(es);
  else if (is_extension(rm_state.fun)) eval_apply_extension(es);
  // TODO: else is an error. Set cont to done 
}

void ec_eval(void) {

  eval_state es = EVAL_DISPATCH;

  bool done = false; 
  
  while (!done) {

    switch(es) {
    case EVAL_DISPATCH:
      /* dispatch on exp */
      switch (kind_of(rm_state.exp)) {
      case EXP_SELF_EVALUATING: eval_self_evaluating(&es); break;
      case EXP_VARIABLE:        eval_variable(&es);        break;
      case EXP_QUOTED:          eval_quoted(&es);          break;
      case EXP_DEFINE:          eval_define(&es);          break;
      case EXP_NO_ARGS:         eval_no_args(&es);         break;
      case EXP_APPLICATION:     eval_application(&es);     break;
      case EXP_LAMBDA:          eval_lambda(&es);          break;
      case EXP_IF:                                         break;
      case EXP_LET:                                        break;
      case EXP_KIND_ERROR:      done = true;               break;   
      }
      break;
    case EVAL_CONTINUATION:
      /* dispatch on cont */
      switch (rm_state.cont) {
      case CONT_DONE:                done = true;                   break;
      case CONT_DEFINE:              cont_define(&es);              break;
      case CONT_SETUP_NO_ARG_APPLY:  cont_setup_no_arg_apply(&es);  break;
      case CONT_EVAL_ARGS:           cont_eval_args(&es);           break;
      case CONT_ACCUMULATE_ARG:      cont_accumulate_arg(&es);      break;
      case CONT_ACCUMULATE_LAST_ARG: cont_accumulate_last_arg(&es); break;
      }
      break;
    case EVAL_ARG_LOOP:        eval_arg_loop(&es);       break;
    case EVAL_LAST_ARG:        eval_last_arg(&es);       break;
    case EVAL_APPLY_DISPATCH:  eval_apply_dispatch(&es); break; 
    }
  }
}
