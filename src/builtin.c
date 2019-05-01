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
bool is_number(VALUE arg) {
  TYPE t = type_of(arg);
  return (t == VAL_TYPE_I ||
	  t == VAL_TYPE_U ||
	  t == PTR_TYPE_BOXED_I ||
	  t == PTR_TYPE_BOXED_U ||
	  t == PTR_TYPE_BOXED_F);
}

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
VALUE bi_fun_car(VALUE args) {
  return car(car(args));
}

VALUE bi_fun_cdr(VALUE args) {
  return cdr(car(args));
}

VALUE bi_fun_cons(VALUE args) {
  VALUE a = car(args);
  VALUE b = car(cdr(args));
  return cons(a,b);
}

VALUE bi_fun_reverse(VALUE args) {
  VALUE xs = car(args);
  return reverse(xs);
}

VALUE bi_fun_sum(VALUE args) {
  VALUE   curr = args;
  INT     i_sum = 0;
  UINT    u_sum = 0;
  FLOAT   f_sum = 0.0;

  UINT tmp;
  UINT float_enc;
  TYPE max_type;

  if (! (get_max_num_type(args, &max_type)))
    return enc_sym(symrepr_terror());

  switch (max_type) {

  case VAL_TYPE_I:
    while(type_of(curr) == PTR_TYPE_CONS) {
      VALUE v = car(curr);
      INT   r;
      get_as_int(v,&r);
      i_sum += r;
      curr = cdr(curr);
    }
    return enc_i(i_sum);

  case VAL_TYPE_U:
    while(type_of(curr) == PTR_TYPE_CONS) {
      VALUE v = car(curr);
      UINT r;
      get_as_uint(v,&r);
      u_sum += r;
      curr = cdr(curr);
    }
    return enc_u(u_sum);

  case PTR_TYPE_BOXED_I:
    while(type_of(curr) == PTR_TYPE_CONS) {
      VALUE v = car(curr);
      INT r;
      get_as_int(v,&r);
      i_sum += r;
      curr= cdr(curr);
    }
    tmp = cons(i_sum,enc_sym(SPECIAL_SYM_BOXED_I));
    return set_ptr_type(tmp, PTR_TYPE_BOXED_I);

  case PTR_TYPE_BOXED_U:
    while(type_of(curr) == PTR_TYPE_CONS) {
      VALUE v = car(curr);
      UINT  r;
      get_as_uint(v,&r);
      u_sum += r;
      curr= cdr(curr);
    }
    tmp = cons(u_sum,enc_sym(SPECIAL_SYM_BOXED_U));
    if (type_of(tmp) == VAL_TYPE_SYMBOL) // an error
      return tmp;
    return set_ptr_type(tmp, PTR_TYPE_BOXED_U);

  case PTR_TYPE_BOXED_F:
    while(type_of(curr) == PTR_TYPE_CONS) {
      VALUE v = car(curr);
      FLOAT r;
      get_as_float(v,&r);
      f_sum += r;
      curr = cdr(curr);
    }
    memcpy(&tmp, &f_sum, sizeof(UINT));
    float_enc = cons(tmp,enc_sym(SPECIAL_SYM_BOXED_F));
    if (type_of(float_enc) == VAL_TYPE_SYMBOL) // an error
      return float_enc;
    float_enc = set_ptr_type(float_enc, PTR_TYPE_BOXED_F);
    return float_enc;
  }

  return enc_sym(symrepr_eerror());
}

VALUE bi_fun_sub(VALUE args) {

  UINT  tmp;
  UINT  enc;
  FLOAT f_res;
  UINT  u_res;
  INT   i_res;
  TYPE  max_type;

  if (!(get_max_num_type(args, &max_type)))
    return enc_sym(symrepr_terror());

  unsigned int n = length(args);

  if (n < 1) return enc_sym(symrepr_eerror());

  // A negation
  if (n == 1) {
    switch (max_type) {

    case VAL_TYPE_I:
      i_res = -dec_i(car(args));
      return enc_i(i_res);

    case VAL_TYPE_U:
      u_res = -dec_u(car(args));
      return enc_u(u_res);

    case PTR_TYPE_BOXED_I:
      i_res = -(INT)car(car(args));
      enc = cons(i_res, enc_sym(SPECIAL_SYM_BOXED_I));
      if (type_of(enc) == VAL_TYPE_SYMBOL) // an error
	return enc;
      enc = set_ptr_type(enc,PTR_TYPE_BOXED_I);
      return enc;

    case PTR_TYPE_BOXED_U:
      u_res = -car(car(args));
      enc = cons(u_res, enc_sym(SPECIAL_SYM_BOXED_U));
      if (type_of(enc) == VAL_TYPE_SYMBOL) // an error
	return enc;
      enc = set_ptr_type(enc,PTR_TYPE_BOXED_U);
      return enc;

    case PTR_TYPE_BOXED_F:
      enc = car(car(args));
      memcpy(&f_res, &enc, sizeof(FLOAT));
      f_res = - f_res;
      memcpy(&tmp, &f_res, sizeof(UINT));
      enc = cons(tmp,enc_sym(SPECIAL_SYM_BOXED_F));
      if (type_of(enc) == VAL_TYPE_SYMBOL) // an error
	return enc;
      enc = set_ptr_type(enc, PTR_TYPE_BOXED_F);
    return enc;
      break;
    }
  }

  VALUE v = car(args);
  VALUE curr = cdr(args);

  switch (max_type) {
  case VAL_TYPE_I:
    get_as_int(v, &i_res);

    while(type_of(curr) == PTR_TYPE_CONS) {
      UINT v = car(curr);
      INT r;
      get_as_int(v,&r);
      i_res -= r;
      curr = cdr(curr);
    }
    return enc_i(i_res);

  case VAL_TYPE_U:
    get_as_uint(v, &u_res);

    while(type_of(curr) == PTR_TYPE_CONS) {
      UINT v = car(curr);
      UINT r;
      get_as_uint(v,&r);
      u_res -= r;
      curr = cdr(curr);
    }
    return enc_u(u_res);

  case PTR_TYPE_BOXED_I:
    get_as_int(v, &i_res);

    while(type_of(curr) == PTR_TYPE_CONS) {
      UINT v = car(curr);
      INT r;
      get_as_int(v, &r);
      i_res -= r;
      curr = cdr(curr);
    }
    enc = cons(i_res, enc_sym(SPECIAL_SYM_BOXED_I));
    if (type_of(enc) == VAL_TYPE_SYMBOL) // an error
      return enc;
    enc = set_ptr_type(enc,PTR_TYPE_BOXED_I);
    return enc;

  case PTR_TYPE_BOXED_U:
    get_as_uint(v, &u_res);

    while(type_of(curr) == PTR_TYPE_CONS) {
      UINT v = car(curr);
      UINT r;
      get_as_uint(v, &r);
      u_res -= r;
      curr = cdr(curr);
    }
    enc = cons(u_res, enc_sym(SPECIAL_SYM_BOXED_U));
    if (type_of(enc) == VAL_TYPE_SYMBOL) // an error
      return enc;
    enc = set_ptr_type(enc,PTR_TYPE_BOXED_U);
    return enc;

  case PTR_TYPE_BOXED_F:
    get_as_float(v, &f_res);

    while(type_of(curr) == PTR_TYPE_CONS) {
      UINT v = car(curr);
      FLOAT r;
      get_as_float(v, &r);
      f_res -= r;
      curr = cdr(curr);
    }
    memcpy(&tmp, &f_res, sizeof(UINT));
    enc = cons(tmp,enc_sym(SPECIAL_SYM_BOXED_F));
    if (type_of(enc) == VAL_TYPE_SYMBOL) // an error
      return enc;
    enc = set_ptr_type(enc, PTR_TYPE_BOXED_F);
    return enc;
  }

  return enc_sym(symrepr_eerror());
}


VALUE bi_fun_gt(VALUE args) {
  VALUE a1 = car(args);
  VALUE a2 = car(cdr(args));
  INT i1, i2;
  UINT u1, u2;
  FLOAT f1, f2;

  TYPE max_type = (type_of(a1) >= type_of(a2)) ? type_of(a1) : type_of(a2);

  switch (max_type) {

  case VAL_TYPE_I:
    get_as_int(a1, &i1);
    get_as_int(a2, &i2);
    if (i1 > i2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case VAL_TYPE_U:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 > u2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case PTR_TYPE_BOXED_I:
    get_as_int(a1, &i1);
    get_as_int(a2, &i2);
    if (i1 > i2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case PTR_TYPE_BOXED_U:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 > u2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case PTR_TYPE_BOXED_F:
    get_as_float(a1, &f1);
    get_as_float(a2, &f2);
    if (f1 > f2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  }

  return enc_sym(symrepr_eerror());
}

VALUE bi_fun_lt(VALUE args) {
  VALUE a1 = car(args);
  VALUE a2 = car(cdr(args));
  INT i1, i2;
  UINT u1, u2;
  FLOAT f1, f2;

  TYPE max_type = (type_of(a1) >= type_of(a2)) ? type_of(a1) : type_of(a2);

  switch (max_type) {

  case VAL_TYPE_I:
    get_as_int(a1, &i1);
    get_as_int(a2, &i2);
    if (i1 < i2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case VAL_TYPE_U:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 < u2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case PTR_TYPE_BOXED_I:
    get_as_int(a1, &i1);
    get_as_int(a2, &i2);
    if (i1 < i2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case PTR_TYPE_BOXED_U:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 < u2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case PTR_TYPE_BOXED_F:
    get_as_float(a1, &f1);
    get_as_float(a2, &f2);
    if (f1 < f2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  }

  return enc_sym(symrepr_eerror());
}

VALUE bi_fun_num_eq(VALUE args) {
  VALUE a1 = car(args);
  VALUE a2 = car(cdr(args));
  INT   i1, i2;
  UINT  u1, u2;
  FLOAT f1, f2;

  TYPE max_type = (type_of(a1) >= type_of(a2)) ? type_of(a1) : type_of(a2);

  switch (max_type) {

  case VAL_TYPE_I:
    get_as_int(a1, &i1);
    get_as_int(a2, &i2);
    if (i1 == i2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case VAL_TYPE_U:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 == u2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case PTR_TYPE_BOXED_I:
    get_as_int(a1, &i1);
    get_as_int(a2, &i2);
    if (i1 == i2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case PTR_TYPE_BOXED_U:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 == u2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  case PTR_TYPE_BOXED_F:
    get_as_float(a1, &f1);
    get_as_float(a2, &f2);
    if (f1 == f2) return enc_sym(symrepr_true());
    else return enc_sym(symrepr_nil());
  }

  return enc_sym(symrepr_eerror());
}


int array_equality(VALUE a, VALUE b) {
  if (type_of(a) == PTR_TYPE_ARRAY &&
      type_of(a) == type_of(b)) {
    array_t *a_ = (array_t*)car(a);
    array_t *b_ = (array_t*)car(b);

    if (a_->elt_type == b_->elt_type &&
	a_->size == b_->size) {
      switch(a_->elt_type) {
      case VAL_TYPE_U:
      case PTR_TYPE_BOXED_U:
	if (memcmp(a_->data.u, b_->data.u, a_->size * sizeof(UINT)) == 0) return 1;
	break;
      case VAL_TYPE_I:
      case PTR_TYPE_BOXED_I:
	if (memcmp(a_->data.i, b_->data.i, a_->size * sizeof(INT)) == 0) return 1;
	break;
      case VAL_TYPE_CHAR:
	if (memcmp(a_->data.c, b_->data.c, a_->size) == 0) return 1;
	break;
      case PTR_TYPE_BOXED_F:
	if (memcmp(a_->data.f, b_->data.f, a_->size * sizeof(FLOAT)) == 0) return 1;
	break;
      default:
	break; 
      }
    }
  }
  return 0; 
}

int structural_equality(VALUE a, VALUE b) {

  if (!is_ptr(a) && !is_ptr(b)) {
    if (val_type(a) == val_type(b)){
      switch (val_type(a)) {
      case VAL_TYPE_SYMBOL:
	if (dec_sym(a) == dec_sym(b)) return 1;
        else return 0;
	break;
      case VAL_TYPE_I:
	if (dec_i(a) == dec_i(b)) return 1;
	else return 0;
	break;
      case VAL_TYPE_U:
	if (dec_u(a) == dec_u(b)) return 1;
	else return 0;
	break;
      case VAL_TYPE_CHAR:
	if (dec_char(a) == dec_char(b)) return 1;
	else return 0;
	break;
      default:
	return 0;
	break;
      }
    } else {
      return 0;
    }
  }

  if (is_ptr(a) && is_ptr(b)) {
    if (ptr_type(a) == ptr_type(b)) {
      switch (ptr_type(a)) {
      case PTR_TYPE_CONS:
	if ( structural_equality(car(a),car(b)) &&
	     structural_equality(cdr(a),cdr(b)) ) return 1;
	return 0;
      case PTR_TYPE_BOXED_I:
	if ((INT)car(a) == (INT)car(b)) return 1;
	return 0;
      case PTR_TYPE_BOXED_U:
	if (car(a) == car(b)) return 1;
	return 0;
      case PTR_TYPE_BOXED_F:
	if ((FLOAT)car(a) == (FLOAT)car(b)) return 1;
	return 0;
      case PTR_TYPE_ARRAY:
	return array_equality(a, b); 
      default:
	printf("TODO: Structural equality for this ptr type not implemented\n");
	return 0;
      }
    }
  }
  return 0;
}
  
VALUE bi_fun_eq(VALUE args) {
  VALUE a1 = car(args);
  VALUE a2 = car(cdr(args));

  return( structural_equality(a1, a2) ? enc_sym(symrepr_true()) : enc_sym(symrepr_nil()) );
}

VALUE bi_fun_gensym(VALUE args) {

  (void)args; // ignores any arguments

  // Takes no arguments!
  UINT gs;
  int res = gensym(&gs);

  if (res) return enc_sym(gs);
  return enc_sym(symrepr_eerror());
}

VALUE bi_fun_list(VALUE args) {
  VALUE t = enc_sym(symrepr_nil());
  VALUE list = enc_sym(symrepr_nil());
  VALUE curr = args;
  while (is_ptr(curr) && ptr_type(curr) == PTR_TYPE_CONS) {
    t = cons(car(curr),t);
    if (type_of(t) == VAL_TYPE_SYMBOL) // an error
      return t;

    curr = cdr(curr);
  }
  curr = t;
  while (is_ptr(curr) && ptr_type(curr) == PTR_TYPE_CONS) {
    list = cons(car(curr),list);
    if (type_of(list) == VAL_TYPE_SYMBOL) // an error
      return list;

    curr = cdr(curr);
  }
  return list;
}

// Built in array functions

VALUE bi_fun_array_read(VALUE args) {
  // Args are: array, index
  VALUE arr = car(args);
  VALUE index = car(cdr(args));

  // Get array index
  UINT ix;
  INT  tmp;
  UINT res = enc_sym(symrepr_eerror());
  switch (type_of(index)) {
  case VAL_TYPE_U:
    ix = dec_u(index);
    break;
  case VAL_TYPE_I:
    tmp = (INT)dec_i(index);
    if (tmp < 0) return enc_sym(symrepr_eerror());
    ix = (UINT)tmp;
    break;
  case PTR_TYPE_BOXED_U:
    ix = car(index);
    break;
  case PTR_TYPE_BOXED_I:
    tmp = (INT)car(index);
    if (tmp < 0) return enc_sym(symrepr_eerror());
    ix = (UINT) tmp;
    break;
  default:
    return enc_sym(symrepr_nil());
  }

  if (type_of(arr) == PTR_TYPE_ARRAY) {
    array_t *array = (array_t*)car(arr);

    if (ix >= array->size) return enc_sym(symrepr_nil());

    switch(array->elt_type) {
    case VAL_TYPE_CHAR:
      res = enc_char((UINT) array->data.c[ix]);
      break;
    case VAL_TYPE_U:
      res = enc_u(array->data.u[ix]);
      break;
    case VAL_TYPE_I:
      res = enc_i(array->data.i[ix]);
      break;
    case PTR_TYPE_BOXED_U:
      res = cons(array->data.u[ix], enc_sym(SPECIAL_SYM_BOXED_U));
      if (type_of(res) == VAL_TYPE_SYMBOL) // an error
	return res;
      res = set_ptr_type(res, PTR_TYPE_BOXED_U);
      break;
    case PTR_TYPE_BOXED_I:
      res = cons(array->data.i[ix], enc_sym(SPECIAL_SYM_BOXED_I));
      if (type_of(res) == VAL_TYPE_SYMBOL) // an error
	return res;
      res = set_ptr_type(res, PTR_TYPE_BOXED_I);
      break;
    case PTR_TYPE_BOXED_F:
      res = cons(array->data.f[ix], enc_sym(SPECIAL_SYM_BOXED_F));
      if (type_of(res) == VAL_TYPE_SYMBOL) // an error
	return res;
      res = set_ptr_type(res, PTR_TYPE_BOXED_F);
      break;
    default:
      printf("unknown type!\n");
      return enc_sym(symrepr_eerror());
    }
  }
  return res;
}

VALUE bi_fun_array_write(VALUE args) {
  // Args are: array, index, value
  VALUE arr = car(args);
  VALUE index = car(cdr(args));
  VALUE val = car(cdr(cdr(args)));
  UINT uv;
  FLOAT v;
  // Get array index
  UINT ix;
  INT tmp;
  switch (type_of(index)) {
  case VAL_TYPE_U:
    ix = dec_u(index);
    break;
  case VAL_TYPE_I:
    tmp = (INT)dec_i(index);
    if (tmp < 0) return enc_sym(symrepr_eerror());
    ix = (UINT) tmp;
    break;
  case PTR_TYPE_BOXED_U:
    ix = car(index);
    break;
  case PTR_TYPE_BOXED_I:
    tmp = (INT)car(index);
    if (tmp < 0) return enc_sym(symrepr_eerror());
    ix = (UINT) tmp;
    break;
  default:
    return enc_sym(symrepr_nil());
  }

  if (type_of(arr) == PTR_TYPE_ARRAY) {
    array_t *array = (array_t*)car(arr);
    if (type_of(val) != array->elt_type) return enc_sym(symrepr_nil());
    if (ix >= array->size) return enc_sym(symrepr_nil());

    switch(array->elt_type) {
    case VAL_TYPE_CHAR:
      array->data.c[ix] = dec_char(val);
      break;
    case VAL_TYPE_U:
      array->data.u[ix] = dec_u(val);
      break;
    case VAL_TYPE_I:
      array->data.i[ix] = dec_i(val);
      break;
    case PTR_TYPE_BOXED_U:
      array->data.u[ix] = car(val);
      break;
    case PTR_TYPE_BOXED_I:
      array->data.i[ix] = (INT)car(val);
      break;
    case PTR_TYPE_BOXED_F:
      uv = car(val);
      memcpy(&v, &uv, sizeof(FLOAT));
      array->data.f[ix] = v;
      break;
    default:
      printf("unknown type!\n");
      return enc_sym(symrepr_eerror());
    }
  }
  return enc_sym(symrepr_true());
}


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
  
  VALUE res = cons(ref, enc_sym(SPECIAL_SYM_REF));
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

  VALUE res = cons(val, enc_sym(SPECIAL_SYM_BOXED_I));
  if (type_of(res) == VAL_TYPE_SYMBOL) 
    return res; // propagate error
  return set_ptr_type(res, PTR_TYPE_BOXED_I);  
}

VALUE bi_fun_ref_read_U(VALUE args) {
  VALUE ref = car(args);
  if (type_of(ref) != PTR_TYPE_REF) return (enc_sym(symrepr_terror()));
  VALUE addr = car(ref);

  UINT val = *(UINT*)addr;

  VALUE res = cons(val, enc_sym(SPECIAL_SYM_BOXED_U));
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

  res &= builtin_add_function("+", bi_fun_sum);
  res &= builtin_add_function("-", bi_fun_sub);
  res &= builtin_add_function("car", bi_fun_car);
  res &= builtin_add_function("cdr", bi_fun_cdr);
  res &= builtin_add_function("cons", bi_fun_cons);
  res &= builtin_add_function(">", bi_fun_gt);
  res &= builtin_add_function("<", bi_fun_lt);
  res &= builtin_add_function("=", bi_fun_eq);
  res &= builtin_add_function("num-eq", bi_fun_num_eq);
  res &= builtin_add_function("gensym", bi_fun_gensym);
  res &= builtin_add_function("list", bi_fun_list);
  res &= builtin_add_function("reverse", bi_fun_reverse);
  res &= builtin_add_function("array-read", bi_fun_array_read);
  res &= builtin_add_function("array-write", bi_fun_array_write);
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
