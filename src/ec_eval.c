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
  EXP_APPLICATION,
  EXP_LET
} exp_kind;

typedef enum {
  CONT_DEFINE,
} continuation;


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
      return EXP_APPLICATION;
    } // end if symbol
  } // end case PTR_TYPE_CONS:
  } 
  return EXP_KIND_ERROR;
}

static inline void eval_self_evaluating(bool *eval_continuation) {
  rm_state.val = rm_state.exp;
  *eval_continuation = true;
}

static inline void eval_variable(bool *eval_continuation) {
  rm_state.val = env_lookup(rm_state.exp, rm_state.env);
  *eval_continuation = true;
}

static inline void eval_quoted(bool *eval_continuation) {
  rm_state.val = car(cdr(rm_state.exp));
  *eval_continuation = true;
}

static inline void eval_define(void) {
  rm_state.unev = car(cdr(rm_state.exp));
  rm_state.exp  = car(cdr(cdr(rm_state.exp)));
  push_u32_3(&rm_state.S,
	     rm_state.unev,
	     rm_state.env,
	     rm_state.cont);
  rm_state.cont = CONT_DEFINE;
}

static inline void cont_define(bool *eval_continuation) {
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
  *eval_continuation = true;
}

static inline void eval_application(void) {
  //  TODO
}

static inline VALUE mkClosure(VALUE exp, VALUE env) {

  VALUE env_end = cons(rm_state.env, enc_sym(symrepr_nil()));
  VALUE body    = cons(car(cdr(cdr(rm_state.exp))), env_end);
  VALUE params  = cons(car(cdr(rm_state.exp)),body);
  VALUE closure = cons(enc_sym(symrepr_closure()), params);

  //TODO: error checking and garbage collection
  return closure;  
}

static inline void eval_lambda(bool *eval_continuation) {

  rm_state.val = mkClosure(rm_state.exp, rm_state.env);
  *eval_continuation = true;
}

void ec_eval(void) {

  bool eval_continuation = false;
  
  while (1) {

    if (!eval_continuation) {
    /* dispatch on exp */
      switch (kind_of(rm_state.exp)) {
      case EXP_SELF_EVALUATING:
	eval_self_evaluating(&eval_continuation);
	break;
      case EXP_VARIABLE:
	eval_variable(&eval_continuation);
	break;
      case EXP_QUOTED:
	eval_quoted(&eval_continuation);
	break;
      case EXP_DEFINE:
	eval_define();
	break;
      case EXP_APPLICATION: 
	eval_application();
	break;
      case EXP_LAMBDA:
	eval_lambda(&eval_continuation);
	break;
      }
    } else {
    /* dispatch on cont */
      switch (rm_state.cont) {
      case CONT_DEFINE:
	cont_define(&eval_continuation);
	break;
      }
    }
  }
}
