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
#include "builtin.h"
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

#define COMPILE_DONE        0x00000001
#define COMPILE_ARG_LIST    0x00000002
#define COMPILE_FUNCTION    0x00000003
#define COMPILE_EMIT_CALL   0x00000004

#define CODE_UNIT_SIZE  256

typedef struct code_unit_s{
  char *code;
  struct code_unit_s *next; 
} code_unit;

typedef struct code_units_s {
  struct code_units_s *next;
  code_unit *unit;
} code_units;

typedef struct {
  int num_constants;
  VALUE constants[MAX_CONSTANTS];
  code_units *code_units; 
} code_gen_state;

int code_units_add_unit(code_units *units, code_unit *unit) {

  code_units *curr = units;
  int index = 0; 
  
  while (curr->next != NULL) {
    curr = curr->next;
    index++;
  }

  index ++;
  curr->next = (code_units *)malloc(sizeof(code_units));
  if (curr->next == NULL) return -1;
  curr->next->unit = unit;
  curr->next->next = NULL;

  return index;  
}

code_unit *code_units_index(code_units *units, int index) {

  if (index < 0) return NULL;

  code_units * curr = units;

  while ( curr != NULL && index > 0 ) {
    curr = curr->next;
    index --;
  }
  if ( curr != NULL && index == 0) return curr->unit;
  return NULL;
}

void code_units_del(code_units *units) {

  code_units *curr = units;
  while (curr != NULL) {
    code_units *tmp = curr->next;
    if (curr->unit->code) free(curr->unit->code);
    if (curr->unit) free(curr->unit);
    if (curr) free(curr);
    curr = tmp;
  }
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

void continuation(stack *s, unsigned int *pc, unsigned char *code, VALUE *curr, bool *done) {

  UINT top;
  pop_u32(s, &top);
  switch(dec_u(top)) {

  case COMPILE_DONE:
    code[*pc] = OP_DONE; *pc = *pc+1;
    *done = true;
    break;
  case COMPILE_ARG_LIST: {
    VALUE rest;
    pop_u32(s,&rest);
    if (type_of(rest) == VAL_TYPE_SYMBOL &&
	dec_sym(rest) == symrepr_nil()) {
      continuation(s, pc, code, curr, done);
    } else {
      VALUE head = car(rest);
      push_u32(s, cdr(rest));
      push_u32(s, enc_u(COMPILE_ARG_LIST));
      *curr = head;
      *done = false;
    }
    break;
  }
  case COMPILE_FUNCTION: {
    VALUE fun;
    pop_u32(s,&fun);
    push_u32(s, enc_u(COMPILE_EMIT_CALL));
    *curr = fun;
    *done = false;
    break;
  }
  case COMPILE_EMIT_CALL: {
    UINT n_args;
    pop_u32(s, &n_args);

    code[*pc] = OP_FUN_APP; *pc = *pc+1;
    code[*pc] = n_args; *pc = *pc+1;

    continuation(s, pc, code, curr, done);
    break;
  }

  default:
    return;
  }
}

int bytecode_create(bytecode_t *bc, int size) {
  bc->code = NULL;
  bc->code = malloc(size);
  if (bc->code == NULL) return 0;
  bc->num_constants = 0;
  return 1;
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

int bytecode_compile(stack *s, VALUE v, bytecode_t *bc, int *err_code) {

  // TODO: Restore SP on error return
  unsigned int pc = 0;
  unsigned int const_ix = 0;

  VALUE   *consts = bc->constants;
  uint8_t *code = bc->code;
  push_u32(s, enc_u(COMPILE_DONE));
  bool done = false;
  VALUE curr = v;
  VALUE head;
  unsigned int n_args;

  *err_code = COMPILER_OK;
  while (!done) {

    curr = try_reduce_constant(curr);

    switch(type_of(curr)) {
    case VAL_TYPE_SYMBOL:
      // Symbols should be compiled into either of:
      //  -1 A a reference to the stack where the value is located
      //  -2 A function name symbol
      //  -3 A value looked up from the environment within which compile was called
      //  - Does 3 make any sense at all??
      //  - 3 is kind of needed to get ids of built in functions.
      //  - But what if a looked up symbol results in a closure, is it sucked in and compiled as well?.
    case VAL_TYPE_U:
    case VAL_TYPE_I: {
      unsigned int ix;
      if ( index_of(consts, const_ix, curr, &ix) ){
	code[pc++] = OP_PUSH_CONST_V;
	code[pc++] = (uint8_t)ix;
      }else if (const_ix < MAX_CONSTANTS) {
	consts[const_ix] = curr;
	code[pc++] = OP_PUSH_CONST_V;
	code[pc++] = (uint8_t)const_ix++;
      } else {
	code[pc++] = OP_PUSH_CONST_D;
	code[pc++] = (uint8_t)curr >> 24;
	code[pc++] = (uint8_t)curr >> 16;
	code[pc++] = (uint8_t)curr >> 8;
	code[pc++] = (uint8_t)curr;
      }
      continuation(s, &pc, code,  &curr, &done);
    }break;
    case PTR_TYPE_CONS:
      head = car(curr);

      // Check for special form symbols
      if (type_of(head) == VAL_TYPE_SYMBOL) {
	if (dec_sym(head) == symrepr_quote()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return 0;
	}
	if (dec_sym(head) == symrepr_define()) {
	  *err_code = ERROR_FORBIDDEN_FORM_DEFINE;
	  return 0;
	}
	if (dec_sym(head) == symrepr_let()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return 0;
	}
	if (dec_sym(head) == symrepr_lambda()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return 0;
	}
	if (dec_sym(head) == symrepr_closure()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return 0;
	}
	if (dec_sym(head) == symrepr_if()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return 0;
	}
      } // end if head is special form symbol
      // possibly function application
      n_args = length(cdr(curr));
      push_u32(s, n_args);
      push_u32(s, head);
      push_u32(s, enc_u(COMPILE_FUNCTION));
      push_u32(s, cdr(cdr(curr)));
      push_u32(s, enc_u(COMPILE_ARG_LIST));

      curr = car(cdr(curr)); //continue and compile first argument
      break;
    default:
      *err_code = ERROR_CANNOT_COMPILE;
      return 0;
    }
  }

  bc->code_size = pc;
  bc->num_constants = const_ix;

  return 1;
}


VALUE bytecode_eval(stack *s, bytecode_t *bc) {

  unsigned int pc=0;
  uint8_t *code = bc->code;
  bool running = true;
  uint8_t ix;
  uint32_t val;
  VALUE fun = 0;
  VALUE hack = enc_sym(symrepr_nil());
  uint8_t n_args = 0;
  bi_fptr bi_fun_ptr = NULL;

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
      val = code[pc++];
      val |= code[pc++] << 8;
      val |= code[pc++] << 16;
      val |= code[pc++] << 24;
      break;
    case OP_FUN_APP: {
      pc++;
      pop_u32(s, &fun);
      n_args = code[pc++];
      for (int i = 0; i < n_args; i ++) {
	pop_u32(s,&val);
	hack = cons(val, hack);
      }
      bi_fun_ptr = builtin_lookup_function(dec_sym(fun));
      if (bi_fun_ptr != NULL) {
	hack = bi_fun_ptr(hack);
	push_u32(s,hack);
      } else {
	return enc_sym(symrepr_eerror());
      }
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
