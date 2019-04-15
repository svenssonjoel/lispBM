
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

#include <stdio.h>

#include "symrepr.h"
#include "heap.h"
#include "print.h"
#include "typedefs.h"

// Copies just the skeleton structure of an environment
// The new "copy" will have pointers to the original key-val bindings.
int env_copy_shallow(VALUE env, VALUE *cpy) {

  VALUE res = enc_sym(symrepr_nil());
  VALUE curr = env;

  while (type_of(curr) == PTR_TYPE_CONS) {
    VALUE key = car(car(curr));
    if (dec_sym(key) != symrepr_nil()) {
      res = cons(car(curr), res);

      // Check for "out of memory"
      if (type_of(res) == VAL_TYPE_SYMBOL &&
	  dec_sym(res) == symrepr_nil()) {
	return 0;
      }
    }
    curr = cdr(curr);
  }
  *cpy = res;
  return 1;
}

int env_lookup(VALUE sym, VALUE env, VALUE *res) {
  VALUE curr = env;

  if(dec_sym(sym) == symrepr_nil()) {
    *res = enc_sym(symrepr_nil());
    return 1;
  }

  while (type_of(curr) == PTR_TYPE_CONS) {
    if (car(car(curr)) == sym) {
      *res = cdr(car(curr));
      return 1;
    }
    curr = cdr(curr);
  }
  return 0;
}


int env_modify_binding(VALUE env, VALUE key, VALUE val) {

  VALUE curr = env;

  while (type_of(curr) == PTR_TYPE_CONS) {
    if (car(car(curr)) == key) {
      set_cdr(car(curr), val);
      return 1;
    }
    curr = cdr(curr);

  }
  return 0;
}


int env_build_params_args(VALUE params,
			  VALUE args,
			  VALUE env0,
			  VALUE *res_env) {
  VALUE curr_param = params;
  VALUE curr_arg = args;

  // TODO: This should be checked outside of this function.
  //
  if (length(params) != length(args)) { // programmer error
    printf("Length mismatch params - args\n");
    simple_print(params); printf("\n");
    simple_print(args); printf("\n");
    return 0;
  }

  VALUE env = env0;
  while (type_of(curr_param) == PTR_TYPE_CONS) {

    VALUE entry = cons(car(curr_param), car(curr_arg));
    if (type_of(entry) == VAL_TYPE_SYMBOL &&
	dec_sym(entry) == symrepr_merror())
      return 0;

    env = cons(entry,env);

    if (type_of(env) == VAL_TYPE_SYMBOL &&
	dec_sym(env) == symrepr_merror())
      return 0;

    curr_param = cdr(curr_param);
    curr_arg   = cdr(curr_arg);
  }
  *res_env = env;
  return 1;
}
