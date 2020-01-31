/*
    Copyright 2019 Joel Svensson	svenssonjoel@yahoo.se

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

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "bytecode.h"
#include "symrepr.h"
#include "env.h"
#include "stack.h"
#include "heap.h"
#include "print.h"
#include "fundamental.h" 
#include "eval_cps.h"

/*
 *  TODO:
 *    - Compile and Eval will work together. Eval will call compile and compile
 *      will call eval.
 *        * Use for constant folding.
 *    - Figure out what the actual interplay between eval and compile will be.
 *    - Figure out how to compile qouted expressions.
 *    - Figure out how to compile functions that return functions.
 *        * Will all cases of this be compilable (make sense)?
 *        * I guess variables need to be "captured" at compile time.
 *        * But it feels like if the returned function depends on values from the 
 *          "outer" function, It should either not be compiled or be compiled when 
 *          the "outer" function is called.
 *        * But dont want too many cases, so if it is easier to just forbid compilataion
 *          of certain things then that is what I will do.
 *    - Figure out how to make tail-calls space efficient.
 *    - I think (maybe) the compiler should have access to environments when compiling. 
 *        * Would mean variables could be looked up and compiled into the bytecode. 
 *        * However, also a bit odd if not the "program" you compile is self contained... 
 *        * This depends, I guess, on how one uses it. Off-line or on-line...
 *    - Optimize for space.
 *    - Change the interface to built-in function (perhaps).
 *    - In order to use as off-line compiler, symbol representations need to
 *      always be the same (independent of platform etc). Currently this is probably not true
 *      as symbols may get different ID's based on order of being added to the system.
 */

#define COMPILE_DONE          0x00000001
#define COMPILE_ARG_LIST      0x00000002
#define COMPILE_FUNCTION_APP  0x00000003
#define COMPILE_EMIT_CALL     0x00000004

#define CODE_REALLOC_STEP   2048
#define FUNCTIONS_REALLOC_STEP 100

char *bytecode_compiler_errors[6] = {"COMPILE_OK",
				     "NOT_ENOUGH_MEMORY",
				     "FORM_NOT_IMPLEMENTED",
				     "FORM_NOT_ALLOWED",
				     "NOT_A_CLOSURE",
				     "CANNOT_COMPILE"};

char *bytecode_get_error(int err) {
  return bytecode_compiler_errors[err];
}

typedef struct {
  uint8_t *code;
  unsigned int code_size;
  unsigned int code_buffer_size;
  unsigned int pc;
} code_buffer;

typedef struct {
  code_buffer **functions; // pointer to array of functions
  unsigned int functions_buffer_size;
  unsigned int num_functions;
  VALUE constants[MAX_CONSTANTS];
  unsigned int constant_ix;
  unsigned int current_function;
} code_gen_state;

unsigned int total_code_size(code_gen_state *gs) {
  unsigned int acc_size = 0;
  for (unsigned int i = 0; i < gs->num_functions; i++) {
    acc_size += gs->functions[i]->code_size;
  }
  return acc_size;
}

bool emit(code_gen_state *gs, uint8_t b) {

  int ix = gs->current_function;

  if (gs->functions[ix]->code_size == gs->functions[ix]->code_buffer_size) {
    uint8_t *new_buffer =
      (uint8_t*)realloc(gs->functions[ix]->code,
			gs->functions[ix]->code_buffer_size +
			CODE_REALLOC_STEP);
    if (new_buffer == NULL) return false;
    gs->functions[ix]->code_buffer_size += CODE_REALLOC_STEP;
    gs->functions[ix]->code = new_buffer;
  }
  gs->functions[ix]->code[gs->functions[ix]->pc] = b;
  gs->functions[ix]->pc = gs->functions[ix]->pc + 1;
  gs->functions[ix]->code_size++;
  return true;
}

bool next_function(code_gen_state *gs, unsigned int *res) {

  if ( gs->num_functions == gs->functions_buffer_size) {
    code_buffer **new_buffer =
      (code_buffer **)realloc(gs->functions,
			      gs->functions_buffer_size +
			      (FUNCTIONS_REALLOC_STEP *
			       sizeof(code_buffer*)));
    if (!new_buffer) return false;
    gs->functions = new_buffer;

  }

  if ( gs->num_functions < gs->functions_buffer_size) {
    *res = gs->num_functions;
    gs->functions[gs->num_functions]->code = (uint8_t*)malloc(CODE_REALLOC_STEP);
    if (!gs->functions[gs->num_functions]->code) return false;
    gs->functions[gs->num_functions]->code_size = 0;
    gs->functions[gs->num_functions]->code_buffer_size = CODE_REALLOC_STEP;
    gs->num_functions++;
  }

  return true;
}

void code_gen_state_del(code_gen_state *gs) {

  for (unsigned int i = 0; i < gs->num_functions; i ++) {
    if (gs->functions[i]->code) free(gs->functions[i]->code);
  }
  free(gs);
}

int index_of(VALUE *constants, unsigned int num, VALUE v, unsigned int *res) {

  for (unsigned int i = 0; i < num; i ++) {
    if (constants[i] == v) {
      *res = i;
      return 1;
    }
  }
  return 0;
}

void continuation(stack *s, code_gen_state *gs, VALUE arg,
		  VALUE *curr, bool *app_cont, bool *compile_function,
		  bool *done) {

  *app_cont = false;
  *done = false;
  *compile_function = false;

  UINT top;
  pop_u32(s, &top);
  switch(dec_u(top)) {

  case COMPILE_DONE:
    emit(gs, OP_DONE);
    *done = true;
    break;
  case COMPILE_ARG_LIST: {
    VALUE rest;

    pop_u32(s,&rest);
    if (type_of(rest) == VAL_TYPE_SYMBOL &&
	dec_sym(rest) == symrepr_nil()) {
      *app_cont = true;
    } else {
      VALUE head = car(rest);
      push_u32(s, cdr(rest));
      push_u32(s, enc_u(COMPILE_ARG_LIST));
      *curr = head;
      *done = false;
    }
    break;
  }
  case COMPILE_FUNCTION_APP: {
    VALUE fun;
    pop_u32(s,&fun);
    push_u32(s, enc_u(COMPILE_EMIT_CALL));
    *compile_function = true;
    *curr = fun;
    break;
  }
  case COMPILE_EMIT_CALL: {
    UINT n_args;
    UINT fun = arg;
    //bi_fptr bi_fun_ptr = NULL;
    pop_u32(s, &n_args);

    if (type_of(fun) == VAL_TYPE_SYMBOL) {
      //bi_fun_ptr = builtin_lookup_function(dec_sym(fun));
      //if (bi_fun_ptr) {
        //emit(gs, OP_BUILTIN_APP);
	//emit(gs, n_args);
	//emit(gs, (uint8_t)((UINT)bi_fun_ptr >> 24));
	//emit(gs, (uint8_t)((UINT)bi_fun_ptr >> 16));
	//emit(gs, (uint8_t)((UINT)bi_fun_ptr >> 8));
	//emit(gs, (uint8_t)((UINT)bi_fun_ptr));
      //}
    }

    *app_cont = true;
    break;
  }

  default:
    return;
  }
}

bool bytecode_create(bytecode_t *bc, int size) {
  bc->code = NULL;
  bc->code = malloc(size);
  if (bc->code == NULL) return false;
  bc->num_constants = 0;
  return true;
}

void bytecode_del(bytecode_t *bc) {
  if (bc) {
    if (bc->code) free(bc->code);
    free(bc);
  }
}

extern VALUE run_eval(eval_context_t *ctx); // TODO
VALUE try_reduce_constant(VALUE exp) {

  eval_context_t *ctx = eval_cps_new_context_inherit_env(exp, exp);

  // TODO: look at exp and return if it is of a form that should
  //       not be evaluated (if those exist)

  VALUE res = run_eval(ctx);

  if (type_of(res) == VAL_TYPE_SYMBOL &&
      (dec_sym(res) == symrepr_eerror() ||
       dec_sym(res) == symrepr_terror() ||
       dec_sym(res) == symrepr_merror())) {
    res =  exp;
  }

  eval_cps_drop_top_context();

  return res;
}

code_gen_state* create_gen_state(void) {
  code_gen_state* state = (code_gen_state *)malloc(sizeof(code_gen_state));
  if(!state) return NULL;

  state->functions = (code_buffer**)malloc(FUNCTIONS_REALLOC_STEP *
					   sizeof(code_buffer*));
  if (!state->functions) {
    free(state);
    return NULL;
  }
  for (int i = 0; i < FUNCTIONS_REALLOC_STEP; i ++) {
    state->functions[i] = (code_buffer*)malloc(sizeof(code_buffer));
    state->functions[i]->code = NULL;
    state->functions[i]->code_size = 0;
    state->functions[i]->code_buffer_size = 0;
    state->functions[i]->pc = 0;
  }
  state->functions_buffer_size = FUNCTIONS_REALLOC_STEP;
  state->num_functions = 1;
  state->current_function = 0;
  state->constant_ix = 0;
  return state;
}

bytecode_t *state_to_bytecode(code_gen_state *gs) {
  unsigned int size = total_code_size(gs);

  bytecode_t *bc = (bytecode_t *)malloc(sizeof(bytecode_t));
  if (!bc) return false;
  bytecode_create(bc, size);

  for (unsigned int i = 0; i < MAX_CONSTANTS; i ++) {
    bc->constants[i] = gs->constants[i];
  }
  bc->code_size = size;
  bc->num_constants = gs->constant_ix;
  unsigned int pc = 0;

  // TODO: figure out how to relocate calls....
  for (unsigned int i = 0; i < gs->num_functions; i ++) {
    code_buffer *code_buf = gs->functions[i];
    for (unsigned int j = 0; j < code_buf->code_size; j++) {
      bc->code[pc++] = code_buf->code[j];
    }
  }
  return bc;
}

bool do_bytecode_compile(code_gen_state *gs, stack *s, VALUE v, int *err_code) {

  // TODO: Restore SP on error return
  // unsigned int pc = 0;
  // unsigned int const_ix = 0;

  //VALUE   *consts = bc->constants;
  //uint8_t *code = bc->code;
  push_u32(s, enc_u(COMPILE_DONE));
  bool done = false;
  bool app_cont = false;
  bool compile_function = false;
  VALUE curr = v;
  VALUE head;
  unsigned int n_args;
  UINT res = 0;

  *err_code = COMPILER_OK;
  while (!done) {

    if (compile_function) {
      if (type_of(curr) == VAL_TYPE_SYMBOL) {

	res = curr;
	app_cont = true;
      } else {
	printf("Not implementated yet.. hack hack\n");
	return false;
      }
    }
    compile_function = false;

    if (app_cont) {
      continuation(s, gs, res, &curr, &app_cont, &compile_function, &done);
      continue;
    }

    //curr = try_reduce_constant(curr);
    switch(type_of(curr)) {
    case VAL_TYPE_SYMBOL:
    case VAL_TYPE_U:
    case VAL_TYPE_I: {
      unsigned int ix;
      if ( index_of(gs->constants, gs->constant_ix, curr, &ix) ){
	emit(gs, OP_PUSH_CONST_V);
	emit(gs, (uint8_t)ix);
      } else if (gs->constant_ix < MAX_CONSTANTS) {
	gs->constants[gs->constant_ix] = curr;
	emit(gs, OP_PUSH_CONST_V);
	emit(gs, (uint8_t)gs->constant_ix++);
      } else {
	emit(gs, OP_PUSH_CONST_D);
	emit(gs, (uint8_t)curr >> 24);
	emit(gs, (uint8_t)curr >> 16);
	emit(gs, (uint8_t)curr >> 8);
	emit(gs, (uint8_t)curr);
      }

      res = 0;
      app_cont = true;
    } continue;
    case PTR_TYPE_CONS:
      head = car(curr);

      // Check for special form symbols
      if (type_of(head) == VAL_TYPE_SYMBOL) {
	if (dec_sym(head) == symrepr_quote()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return false;
	}
	if (dec_sym(head) == symrepr_define()) {
	  *err_code = ERROR_FORBIDDEN_FORM;
	  return false;
	}
	if (dec_sym(head) == symrepr_let()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return false;
	}
	if (dec_sym(head) == symrepr_lambda()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return false;
	}
	if (dec_sym(head) == symrepr_closure()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return false;
	}
	if (dec_sym(head) == symrepr_if()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return false;
	}
      } // end if head is special form symbol
      // possibly function application
      n_args = length(cdr(curr));
      push_u32(s, n_args);
      push_u32(s, head);
      push_u32(s, enc_u(COMPILE_FUNCTION_APP));
      push_u32(s, cdr(cdr(curr)));
      push_u32(s, enc_u(COMPILE_ARG_LIST));

      curr = car(cdr(curr)); //continue and compile first argument
      break;
    default:
      *err_code = ERROR_CANNOT_COMPILE;
      return false;
    }
  }
  return true;
}

bytecode_t *bytecode_compile( stack *s, VALUE v, int *err_code) {

  /* if (type_of(car(v)) != VAL_TYPE_SYMBOL || */
  /*     dec_sym(car(v)) != symrepr_closure()) { */
  /*   *err_code = ERROR_NOT_A_CLOSURE; */
  /*   return NULL; */
  /* } */

  code_gen_state *gs;

  gs = create_gen_state();
  if (!gs) return NULL;

  if (!do_bytecode_compile(gs, s, v, err_code)) return NULL;

  bytecode_t *bc;
  bc = state_to_bytecode(gs);
  return bc;
}

VALUE bytecode_eval(stack *s, bytecode_t *bc) {

  unsigned int pc=0;
  uint8_t *code = bc->code;
  bool running = true;
  uint8_t ix;
  uint32_t val;
  VALUE hack = enc_sym(symrepr_nil());
  uint8_t n_args = 0;
  //  bi_fptr bi_fun_ptr = NULL;

  while(running) {
    switch(code[pc]) {
    case OP_PUSH_CONST_V:
      pc++;
      ix = code[pc++];
      push_u32(s, bc->constants[ix]);
      break;
    case OP_PUSH_CONST_D:
      val = 0;
      pc++;
      val  = code[pc++] << 24;
      val |= code[pc++] << 16;
      val |= code[pc++] << 8;
      val |= code[pc++];
      push_u32(s, val);
      break;
    case OP_BUILTIN_APP: {
      UINT tmp;
      pc++;
      n_args = code[pc++];
      tmp  = code[pc++] << 24;
      tmp |= code[pc++] << 16;
      tmp |= code[pc++] << 8;
      tmp |= code[pc++];
      //bi_fun_ptr = (bi_fptr)tmp;
      for (int i = 0; i < n_args; i ++) {
	pop_u32(s,&val);
	hack = cons(val, hack);
      }
      //if (bi_fun_ptr != NULL) {
      //	hack = bi_fun_ptr(hack);
      //	push_u32(s,hack);
      //} else {
	return enc_sym(symrepr_eerror());
	//}
    }break;
    case OP_DONE:
      running = false;
      break;
    case OP_RETURN:
      running = false;
      break;
    default:
      return enc_sym(symrepr_eerror());
    }
  }
  VALUE v;
  pop_u32(s, &v);
  return v;
}
