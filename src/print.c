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
#include <string.h>
#include <inttypes.h>

#include "print.h"
#include "heap.h"
#include "symrepr.h"
#include "typedefs.h"


int is_closure(VALUE t) {

  VALUE head = car(t);

  if (!is_ptr(head)  && val_type(head) == VAL_TYPE_SYMBOL &&
      dec_sym(head) == symrepr_closure()) {
    return 1;
  }
  return 0;
}

int simple_print_env(VALUE env) {
  VALUE curr = env;

  VALUE a;
  VALUE b;
  printf("(");
  while (is_ptr(curr) && ptr_type(curr) == PTR_TYPE_CONS) {
    VALUE head = car(curr);
    if (is_ptr(head)) {
      a = car(car(head));
      b = cdr(car(head));
      printf("(");
      simple_print(a); printf(" . ");
      if (is_closure(b))
	printf("CLOSURE");
      else simple_print(b);
      printf(") ");
    }
    curr = cdr(curr);
  }
  printf(")");
  return 1;
}


int simple_print_lambda(VALUE t) {

  VALUE lam  = car(t);
  VALUE vars = car(cdr(t));
  VALUE exp  = car(cdr(cdr(t)));
  VALUE env  = car(cdr(cdr(cdr(t))));

  printf("(");
  simple_print(lam); printf(" ");
  simple_print(vars); printf(" ");
  simple_print(exp); printf(" ");
  simple_print_env(env); printf(" ");
  return 1;
}


int simple_print(VALUE t){

  char *str_ptr;

  if (is_ptr(t) && (ptr_type(t) == PTR_TYPE_CONS)) {
    // TODO: Switch on the type of object pointed to.

    VALUE car_val = car(t);

    if (dec_sym(car_val) == symrepr_lambda()) {
      simple_print_lambda(t);
    } else if ((t & PTR_TYPE_MASK) == PTR_TYPE_CONS) {
      printf("(");
      simple_print(car(t));
      printf(" ");
      simple_print(cdr(t));
      printf(")");
    }
    return 1;
  }

  if (is_ptr(t) && ptr_type(t) == PTR_TYPE_BOXED_F) {
    VALUE uv = car(t);
    float v;
    memcpy(&v, &uv, sizeof(float)); // = *(float*)(&uv);
    printf("{%"PRI_FLOAT"}", v);
    return 1;
  }

  if (is_ptr(t) && ptr_type(t) == PTR_TYPE_BOXED_U) {
    VALUE v = car(t);
    printf("{%"PRI_UINT"}", v);
    return 1;
  }

  if (is_ptr(t) && ptr_type(t) == PTR_TYPE_BOXED_I) {
    int32_t v = (int32_t)car(t);
    printf("{%"PRI_INT"}", v);
    return 1;
  }

  if (is_ptr(t) && ptr_type(t) == PTR_TYPE_ARRAY) {
    array_t *array = (array_t *)car(t);
    switch (array->elt_type){
    case VAL_TYPE_CHAR:
      printf("\"%s\"", array->data.c);
      break;
    default:
      printf("Array type not supported\n");
      break;
    }
  }

  if (!is_ptr(t)) { // Value, symbol
    switch (type_of(t)) {
    case VAL_TYPE_SYMBOL:
      str_ptr = symrepr_lookup_name(dec_sym(t));
      if (str_ptr == NULL) {
	printf("Error: Symbol not in table %"PRI_UINT"\n", dec_sym(t));
      } else {
	printf("%s", str_ptr);
      }
      break;
    case VAL_TYPE_I:
      printf("%"PRI_INT"", dec_i(t));
      break;
    case VAL_TYPE_U:
      printf("%"PRI_UINT"", dec_u(t));
      break;
    case VAL_TYPE_CHAR: // dont yet have syntax for Char and String
      printf("\\#%c", dec_char(t));
      break;
    default:
      printf("simple_print: Error\n");
      return 0;
    }
  }
  return 1;
}
