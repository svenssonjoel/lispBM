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
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "symrepr.h"
#include "heap.h"
#include "builtin.h"
#include "print.h"

typedef struct s_builtin_function{
  VALUE sym;
  bi_fptr fun_ptr;
  struct s_builtin_function* next;
} builtin_function_t;

builtin_function_t* function_list = NULL;

////////////////////////////////////////////////////////////
// Built in predicates (numberp, symbolp and such)
/* bool is_number(VALUE arg) { */
/*   TYPE t = type_of(arg); */
/*   return (t == VAL_TYPE_I || */
/* 	  t == VAL_TYPE_U || */
/* 	  t == PTR_TYPE_BOXED_I || */
/* 	  t == PTR_TYPE_BOXED_U || */
/* 	  t == PTR_TYPE_BOXED_F); */
/* } */

VALUE bi_fun_numberp(VALUE args) {
  if (is_number(car(args)) && length(args) == 1) {
    return enc_sym(symrepr_true());
  }
  return enc_sym(symrepr_nil());
}

////////////////////////////////////////////////////////////
// Utility functions
/*
 * get_max_num_type
 * Returns: success or failure (1 or 0).
 * Result is stored in argument 2.
 */
int get_max_num_type(VALUE args, TYPE *type) {

  TYPE max_type = 0;
  VALUE curr = args;

  while ( type_of(curr) == PTR_TYPE_CONS ) {
    if (!is_number(car(curr)))
      return 0;

    if ( type_of(car(curr)) > max_type) {
      max_type = type_of(car(curr));
    }
    curr = cdr(curr);
  }
  *type = max_type;
  return 1;
}

int get_as_int(VALUE v, INT *r) {
  VALUE tmp;
  FLOAT f_tmp;
  switch (type_of(v)) {
  case VAL_TYPE_I:
    *r =  dec_i(v);
    break;
  case VAL_TYPE_U:
    *r = (INT)dec_u(v);
    break;
  case PTR_TYPE_BOXED_I:
    *r = (INT)car(v);
    break;
  case PTR_TYPE_BOXED_U:
    *r = (INT)car(v);
    break;
  case PTR_TYPE_BOXED_F:
    tmp = car(v);
    // f_tmp = *(FLOAT*)&tmp;
    memcpy(&f_tmp, &tmp, sizeof(FLOAT));
    *r = (INT)f_tmp;
    break;
  default:
    return 0;
  }
  return 1;
}

int get_as_uint(VALUE v, UINT *r) {
  VALUE tmp;
  FLOAT    f_tmp;
  switch (type_of(v)) {
  case VAL_TYPE_I:
    *r =  (UINT)dec_i(v);
    break;
  case VAL_TYPE_U:
    *r = dec_u(v);
    break;
  case PTR_TYPE_BOXED_I:
    *r = (UINT)car(v);
    break;
  case PTR_TYPE_BOXED_U:
    *r = car(v);
    break;
  case PTR_TYPE_BOXED_F:
    tmp = car(v);
    // f_tmp = *(FLOAT*)&tmp;
    memcpy(&f_tmp, &tmp, sizeof(FLOAT));
    *r = (UINT)f_tmp;
    break;
  default:
    return 0;
  }
  return 1;
}

int get_as_float(VALUE v, FLOAT *r) {
  VALUE tmp;
  switch (type_of(v)) {
  case VAL_TYPE_I:
    *r =  (FLOAT)dec_i(v);
    break;
  case VAL_TYPE_U:
    *r = (FLOAT)dec_u(v);
    break;
  case PTR_TYPE_BOXED_I:
    *r = (FLOAT)((INT)car(v));
    break;
  case PTR_TYPE_BOXED_U:
    *r = (FLOAT)car(v);
    break;
  case PTR_TYPE_BOXED_F:
    tmp = car(v);
    //*r = *(FLOAT*)&tmp;
    memcpy(r, &tmp, sizeof(FLOAT));
    break;
  default:
    return 0;
  }
  return 1;
}


////////////////////////////////////////////////////////////
// Built in functions

VALUE bi_fun_array_concat(VALUE args) {

  VALUE a0 = car(args); 
  VALUE a1 = car(cdr(args)); 

  if (type_of(a0) != PTR_TYPE_ARRAY ||
      type_of(a1) != PTR_TYPE_ARRAY) {
    return enc_sym(symrepr_terror());
  }

  array_t *arr0 = (array_t *)car(a0);
  array_t *arr1 = (array_t *)car(a1);

  if (arr0->elt_type != arr1->elt_type) {
    return enc_sym(symrepr_terror());
  }

  int new_size =
    ((arr0->elt_type == VAL_TYPE_CHAR) ?
     (arr0->size -1) :
     arr0->size)  + arr1->size;
  VALUE res;
  
  if (!heap_allocate_array(&res, new_size, arr0->elt_type)) {
    return enc_sym(symrepr_merror());
  }

  array_t *array = (array_t*)car(res);
  switch (arr0->elt_type) {
  case PTR_TYPE_BOXED_I:
  case VAL_TYPE_I:
    memcpy(array->data.i, arr0->data.i, arr0->size * sizeof(INT));
    memcpy(array->data.i + arr0->size * sizeof(INT),
	   arr1->data.i, arr1->size * sizeof(INT));
    break; 
  case VAL_TYPE_SYMBOL: 
  case PTR_TYPE_BOXED_U:
  case VAL_TYPE_U:
    memcpy(array->data.u, arr0->data.u, arr0->size * sizeof(UINT));
    memcpy(array->data.u + arr0->size * sizeof(UINT),
	   arr1->data.u, arr1->size * sizeof(UINT));
    break;
  case VAL_TYPE_CHAR: // Only makes sense for strings
                      // but maybe that will be the only allowed Char array?
    memcpy(array->data.c, arr0->data.c, (arr0->size -1) * sizeof(char));
    memcpy(array->data.c + (arr0->size -1) * sizeof(char),
	   arr1->data.c, arr1->size * sizeof(char));
    break;
  case PTR_TYPE_BOXED_F:
    memcpy(array->data.f, arr0->data.f, arr0->size * sizeof(FLOAT));
    memcpy(array->data.f + arr0->size * sizeof(FLOAT),
	   arr1->data.f, arr1->size * sizeof(FLOAT));
    break;
  default:
    return enc_sym(symrepr_eerror());
  }

  return res;
}

////////////////////////////////////////////////////////////
// Built in String manipulation and printing

VALUE bi_fun_to_string(VALUE args) {

  char str[1024];

  if (type_of(args) != PTR_TYPE_CONS) {
    // Empty list of arguments.
    return enc_sym(symrepr_eerror());
  }

  // extract first argument (ignore rest)
  VALUE arg = car(args);

  if (type_of(arg) == PTR_TYPE_ARRAY) {
    array_t *array = (array_t *)car(args);
    if (array->elt_type == VAL_TYPE_CHAR) {
      return args;
    } else {
      // TODO: generate a string representation of array.
    }
  }


  INT   i;
  UINT  u;
  FLOAT f;
  char  c;

  switch (type_of(arg)) {
  case VAL_TYPE_SYMBOL:
    u = dec_sym(arg);
    snprintf(str,1024,"%s", symrepr_lookup_name(u));
    break;
  case VAL_TYPE_I:
    i = dec_i(arg);
    snprintf(str,1024,"%"PRI_INT,i);
    break;
  case VAL_TYPE_U:
    u = dec_u(arg);
    snprintf(str,1024,"%"PRI_UINT,u);
    break;
  case VAL_TYPE_CHAR:
    c = dec_char(arg);
    snprintf(str,1024,"%c",c);
    break;
  case PTR_TYPE_BOXED_F:
    f = dec_f(arg);
    snprintf(str,1024,"%"PRI_FLOAT,f);
    break;
  case PTR_TYPE_BOXED_U:
    u = dec_U(arg);
    snprintf(str,1024,"%"PRI_UINT,u);
    break;
  case PTR_TYPE_BOXED_I:
    i = dec_I(arg);
    snprintf(str,1024,"%"PRI_INT,i); 
    break;
  default:
    return enc_sym(symrepr_eerror());
  }

  unsigned int size = strlen(str);
  VALUE arr;

  if (!heap_allocate_array(&arr, size+1, VAL_TYPE_CHAR)) {
    return enc_sym(symrepr_merror());
  }

  array_t *array = (array_t*)car(arr);

  memset(array->data.c, 0, (size+1) * sizeof(char));
  memcpy(array->data.c, str, size * sizeof(char));

  return arr;
}

VALUE bi_fun_print_str(VALUE args) {

  VALUE curr = args;
  
  while (type_of(curr) == PTR_TYPE_CONS) { 
    VALUE str = car(curr);
    
    if (type_of(str) != PTR_TYPE_ARRAY) {
      return enc_sym(symrepr_terror());
    }

    array_t *arr = (array_t*)car(str);

    if (arr->elt_type != VAL_TYPE_CHAR) {
      return enc_sym(symrepr_terror());
    }

    printf("%s", arr->data.c);
    curr = cdr(curr);
  }  
  return enc_sym(symrepr_nil()); 
}

////////////////////////////////////////////////////////////
// References (Low level direct memory manipulation)

VALUE bi_fun_ref_address(VALUE args) {

  VALUE arg = car(args); // expects only one argument
  if (type_of(arg) != PTR_TYPE_BOXED_U) return enc_sym(symrepr_terror());

  UINT ref = dec_U(arg); 
  
  VALUE res = cons(ref, enc_sym(DEF_REPR_REF_TYPE));
  if (type_of(res) == VAL_TYPE_SYMBOL)
    return res;
  return set_ptr_type(res,PTR_TYPE_REF);
}

// TODO: Maybe pass an "interpret as this type" argument instead of
//       code duplication.
VALUE bi_fun_ref_read_I(VALUE args) {
  VALUE ref = car(args);
  if (type_of(ref) != PTR_TYPE_REF) return (enc_sym(symrepr_terror()));
  VALUE addr = car(ref);

  INT val = *(INT*)addr;

  VALUE res = cons(val, enc_sym(DEF_REPR_BOXED_I_TYPE));
  if (type_of(res) == VAL_TYPE_SYMBOL) 
    return res; // propagate error
  return set_ptr_type(res, PTR_TYPE_BOXED_I);  
}

VALUE bi_fun_ref_read_U(VALUE args) {
  VALUE ref = car(args);
  if (type_of(ref) != PTR_TYPE_REF) return (enc_sym(symrepr_terror()));
  VALUE addr = car(ref);

  UINT val = *(UINT*)addr;

  VALUE res = cons(val, enc_sym(DEF_REPR_BOXED_U_TYPE));
  if (type_of(res) == VAL_TYPE_SYMBOL) 
    return res; // propagate error
  return set_ptr_type(res, PTR_TYPE_BOXED_U);  
}

VALUE bi_fun_ref_write(VALUE args) {

  VALUE ref = car(args);
  VALUE val = car(cdr(args));
  if (type_of(ref != PTR_TYPE_REF)) return (enc_sym(symrepr_terror()));

  UINT addr = car(ref);
  
  union {
    UINT val_u;
    INT val_i;
  } un;
  
  
  switch (type_of(val)) {
  case VAL_TYPE_I:
    un.val_i = dec_i(val);
    break;
  case VAL_TYPE_U:
    un.val_u = dec_u(val);
    break;
  case PTR_TYPE_BOXED_U:
    un.val_u = dec_U(val);
    break;
  case PTR_TYPE_BOXED_I:
    un.val_i = dec_I(val);
    break;
  default:
    return enc_sym(symrepr_terror());
  }
  *(UINT*)addr = un.val_u;
  return enc_sym(symrepr_nil());
}


////////////////////////////////////////////////////////////
// Interface functions

bi_fptr builtin_lookup_function(UINT sym){
  builtin_function_t *t = function_list;

  while (t != NULL) {
    if ( t->sym == sym ) {
      return t->fun_ptr;
    }
    t = t->next;
  }
  return NULL;
}

int builtin_add_function(char *sym_str, bi_fptr fun_ptr){

  VALUE symbol;
  int res = symrepr_addsym(sym_str, &symbol);

  if ( !res ) {
    return 0;
  }

  builtin_function_t *bi = malloc(sizeof(builtin_function_t));

  if ( !bi ) return 0;

  bi->sym = symbol;
  bi->fun_ptr = fun_ptr;
  bi->next = function_list;

  function_list = bi;
  return 1;
}

int builtin_init(void) {
  int res = 1;

  res &= builtin_add_function("array-concat", bi_fun_array_concat);
  res &= builtin_add_function("numberp", bi_fun_numberp);
  res &= builtin_add_function("to-string", bi_fun_to_string);
  res &= builtin_add_function("print-string", bi_fun_print_str);
  res &= builtin_add_function("ref-address", bi_fun_ref_address);
  res &= builtin_add_function("ref-read-I", bi_fun_ref_read_I);
  res &= builtin_add_function("ref-read-U", bi_fun_ref_read_U);
  res &= builtin_add_function("ref-write", bi_fun_ref_write);
  return res;
}

void builtin_del(void) {
  builtin_function_t *curr = function_list;
  builtin_function_t *t;
  while (curr) {
    t = curr;
    curr = curr->next;
    free(t);
  }
  function_list = NULL;
}

VALUE built_in_gen_env(void) {

  builtin_function_t* curr = function_list;

  VALUE env = enc_sym(symrepr_nil());

  while (curr) {
    VALUE sym = enc_sym(curr->sym);
    env = cons(cons(sym,sym),env);
    if (type_of(env) == VAL_TYPE_SYMBOL) {
      return enc_sym(symrepr_merror());
    }
    curr = curr->next;
  }

  return env;
}
