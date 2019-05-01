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
      a = car(head);
      b = cdr(head);
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

  if (is_ptr(t) && ptr_type(t) == PTR_TYPE_REF) {
    printf("_ref_");
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


int simple_snprint_env(char *buf, int len, VALUE env) {
  VALUE curr = env;

  VALUE a;
  VALUE b;
  int acc = 0;
  int n = 0;
  
  n = snprintf(buf, len, "(");
  if (n == 0) return 0;
  acc += n;
  while (is_ptr(curr) && ptr_type(curr) == PTR_TYPE_CONS) {
    VALUE head = car(curr);
    if (is_ptr(head)) {
      a = car(head);
      b = cdr(head);
      
      n = snprintf(buf+acc, len-acc, "(");
      if (n == 0) return acc;
      acc += n;
      
      n = simple_snprint(buf+acc,len-acc, a);
      if (n == 0) return acc;
      acc += n; 

      n = snprintf(buf+acc, len-acc, " . ");
      if (n == 0) return acc;
      acc += n;
      
      if (is_closure(b)) { 
	n = snprintf(buf+acc, len-acc, "_clo_");
	if (n == 0) return acc;
	acc += n;
      }
      else {
	n = simple_snprint(buf+acc, len-acc, b);
	if (n == 0) return acc;
	acc+=n;
      }
      n = snprintf(buf+acc,len-acc, ") ");
      if (n == 0) return acc;
      acc+=n;
    }
    curr = cdr(curr);
  }
  n = snprintf(buf+acc,len-acc,")");
  acc += n; 
  return acc;
}



int simple_snprint_closure(char *buf, int len, VALUE t) {

  VALUE clo  = car(t);
  VALUE vars = car(cdr(t));
  VALUE exp  = car(cdr(cdr(t)));
  VALUE env  = car(cdr(cdr(cdr(t))));

  int n0, n1, n2, n3, n4, n5, n6, n7;

  n0 = snprintf(buf, len, "(");
  if (n0 == 0) return 0;
  n1 = simple_snprint(buf+=n0,len-=n0,clo);
  if (n1 == 0) return n0;
  n2 = snprintf(buf+=n1, len-=n1, " ");
  if (n2 == 0) return n0+n1;
  n3 = simple_snprint(buf+=n2, len-=n2, vars);
  if (n3 == 0) return n0+n1+n2;
  n4 = snprintf(buf+=n3,len-=n3, " ");
  if (n4 == 0) return n0+n1+n2+n3;
  n5 = simple_snprint(buf+=n4, len-=n4, exp);
  if (n5 == 0) return n0+n1+n2+n3+n4;
  n6 = snprintf(buf+=n5,len-=n5, " ");
  if (n6 == 0) return n0+n1+n2+n3+n4+n5;
  n7 = simple_snprint_env(buf+=n6, len-=n6, env);
  if (n7 == 0) return n0+n1+n2+n3+n4+n5+n6;

  return n0+n1+n2+n3+n4+n5+n6+n7;
}

int simple_snprint(char *buf, int len, VALUE t) {
  char *str_ptr;
  int n;

  if (len == 0) {
    return 0;
  }
  
  char *p = buf;

  switch(type_of(t)){

  case PTR_TYPE_CONS:{
    VALUE car_val = car(t);

    if (dec_sym(car_val) == symrepr_closure()) {
      n = simple_snprint_closure(p, len, t);
      return n;
    } else {
      int n0, n1, n2, n3, n4;
      n0 = snprintf(p, len, "(");
      if (n0 == 0) return 0;
      n1 = simple_snprint(p += n0, len-n0, car(t));
      if (n1 == 0) return n0; 
      n2 = snprintf(p+=n1, len-n0-n1, " ");
      if (n2 == 0) return n0+n1; 
      n3 = simple_snprint(p+=n2, len-n0-n1-n2, cdr(t));
      if (n3 == 0) return n0+n1+n2;
      n4 = snprintf(p+=n3, len-n0-n1-n2-n3, ")");
      return n0+n1+n2+n3+n4;
    }
    break;
  }
  case PTR_TYPE_REF:
    n = snprintf(p, len, "_ref_");
    return n;
  case PTR_TYPE_BOXED_F: {
    VALUE uv = car(t);
    float v;
    memcpy(&v, &uv, sizeof(float)); // = *(float*)(&uv);
    return snprintf(p, len, "{%"PRI_FLOAT"}", v);
  }
  case PTR_TYPE_BOXED_U: {
    VALUE v = car(t);
    return snprintf(p, len, "{%"PRI_UINT"}", v);
  }
  case PTR_TYPE_BOXED_I: {
    int32_t v = (int32_t)car(t);
    return snprintf(p, len, "{%"PRI_INT"}", v);
  }
  case PTR_TYPE_ARRAY: {
    array_t *array = (array_t *)car(t);
    switch (array->elt_type){
    case VAL_TYPE_CHAR:
      return snprintf(p, len, "\"%s\"", array->data.c);
    default:
      return snprintf(p, len, "Array type not supported\n");
    }
  }
  case VAL_TYPE_SYMBOL:
    str_ptr = symrepr_lookup_name(dec_sym(t));
    if (str_ptr == NULL) {
      return snprintf(p, len, "Error: Symbol not in table %"PRI_UINT"", dec_sym(t));
    } else {
      return snprintf(p, len, "%s", str_ptr);
    }
  case VAL_TYPE_I:
    return snprintf(p, len, "%"PRI_INT"", dec_i(t));
  case VAL_TYPE_U:
    return snprintf(p, len, "%"PRI_UINT"", dec_u(t));
  case VAL_TYPE_CHAR:
    return snprintf(p, len, "\\#%c", dec_char(t));
  default:
    break;
  }
  return snprintf(p,len,"simple_print: Error\n");
}
