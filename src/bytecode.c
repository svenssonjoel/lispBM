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

/* 
 *  TODO: 
 *    - Compile and Eval will work together. Eval will call compile and compile 
 *      will call eval. Set up a callback function to access eval functionality 
 *      from bytecode compiler. 
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

#define COMPILE_DONE        0xFF000001
#define COMPILE_ARG_LIST    0xFF000002
#define COMPILE_FUNCTION    0xFF000003
#define COMPILE_EMIT_CALL   0xFF000004

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
  switch(top) {

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
      push_u32(s, COMPILE_ARG_LIST);
      *curr = head;
      *done = false;
    }
    break;
  }
  case COMPILE_FUNCTION: {
    VALUE fun;
    pop_u32(s,&fun);
    push_u32(s, COMPILE_EMIT_CALL);
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

int bytecode_ncompile(VALUE v, bytecode_t *bc, int max_size, int *err_code) {

  unsigned int pc = 0;
  unsigned int const_ix = 0;

  VALUE   *consts = bc->constants;
  uint8_t *code = bc->code;
  stack *s = init_cont_stack(100);
  push_u32(s, COMPILE_DONE);
  bool done = false;
  VALUE curr = v;
  VALUE head;
  unsigned int n_args;

  *err_code = COMPILER_OK;

  while (!done) {
    switch(type_of(curr)) {
    case VAL_TYPE_SYMBOL:
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
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
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
	if (dec_sym(head) == symrepr_if()) {
	  *err_code = ERROR_FORM_NOT_IMPLEMENTED;
	  return 0;
	}

      } // end if head is special form symbol
      // possibly function application
      n_args = length(cdr(curr));
      push_u32(s, n_args);
      push_u32(s, head);
      push_u32(s, COMPILE_FUNCTION);
      push_u32(s, cdr(cdr(curr)));
      push_u32(s, COMPILE_ARG_LIST);

      curr = (car(cdr(curr))); //continue and compile first argument
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


VALUE bytecode_eval(bytecode_t bc, VALUE globalenv, VALUE localenv) {

  stack *s = init_cont_stack(100);

  unsigned int pc=0;
  //unsigned int code_size = bc.code_size;
  uint8_t *code = bc.code;
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
      push_u32(s, bc.constants[ix]);
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
	printf("Error: function not found\n");
      }
    }break;
    case OP_DONE:
      running = false;
      break;
    case OP_RETURN:
      break;
    default:
      return enc_sym(symrepr_eerror());
    }
  }
  VALUE v;
  pop_u32(s, &v);
  return v;
}
