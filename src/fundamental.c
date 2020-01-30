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

#include "symrepr.h"
#include "stack.h"
#include "heap.h"
#include "eval_cps.h"
#include "print.h"

#include <stdio.h>
/*
  Fundamental operations expect number of arguments and arguments pushed
  onto the stack.
  | n-args |
  | arg1   |
  | ...    |
  | argn   |
  After execution of a fundamental operation the result will be located
  at the top of the K-stack.
 */

static UINT as_i(UINT a) {

  UINT tmp;
  FLOAT f_tmp;

  switch (type_of(a)) {
  case VAL_TYPE_I:
    return dec_i(a);
  case VAL_TYPE_U:
    return (INT) dec_u(a);
  case PTR_TYPE_BOXED_I:
  case PTR_TYPE_BOXED_U:
    return (INT)car(a);
  case PTR_TYPE_BOXED_F:
    tmp = car(a);
    memcpy(&f_tmp, &tmp, sizeof(FLOAT));
    return (INT)f_tmp;
  }
  return 0;
}

static UINT as_u(UINT a) {

  UINT tmp;
  FLOAT f_tmp;

  switch (type_of(a)) {
  case VAL_TYPE_I:
    return (UINT) dec_i(a);
  case VAL_TYPE_U:
    return dec_u(a);
  case PTR_TYPE_BOXED_I:
  case PTR_TYPE_BOXED_U:
    return (UINT)car(a);
  case PTR_TYPE_BOXED_F:
    tmp = car(a);
    memcpy(&f_tmp, &tmp, sizeof(FLOAT));
    return (UINT)f_tmp;
  }
  return 0;
}

static UINT as_f(UINT a) {

  UINT tmp;
  FLOAT f_tmp;

  switch (type_of(a)) {
  case VAL_TYPE_I:
    return (FLOAT) dec_i(a);
  case VAL_TYPE_U:
    return (FLOAT)dec_u(a);
  case PTR_TYPE_BOXED_I:
  case PTR_TYPE_BOXED_U:
    return (FLOAT)car(a);
  case PTR_TYPE_BOXED_F:
    tmp = car(a);
    memcpy(&f_tmp, &tmp, sizeof(FLOAT));
    return f_tmp;
  }
  return 0;
}

static UINT add2(UINT a, UINT b) {

  UINT retval = enc_sym(symrepr_terror());
  UINT t_min;
  UINT t_max;
  INT i0;
  INT i1;
  UINT u0;
  UINT u1;
  FLOAT f0;
  FLOAT f1;

  if (type_of(a) < type_of(b)) {
    t_min = a;
    t_max = b;
  } else {
    t_min = b;
    t_max = a;
  }

  if (!is_number(t_min)) enc_sym(symrepr_nil());
 
  switch (type_of(t_max)) {
  case VAL_TYPE_I:
    i0 = dec_i(t_max);
    i1 = as_i(t_min);
    retval = enc_i(i0 + i1);
    break;
  case VAL_TYPE_U:
    u0 = dec_u(t_max);
    u1 = as_u(t_min);
    retval = enc_u(u0 + u1);
    break;
  case PTR_TYPE_BOXED_U:
    u0 = dec_U(t_max);
    u1 = as_u(t_min);
    retval = enc_U(u0+u1); //cons(u0+u1, enc_sym(DEF_REPR_BOXED_U_TYPE));
    break;
  case PTR_TYPE_BOXED_I:
    i0 = dec_I(t_max);
    i1 = as_u(t_min);
    retval = enc_I(i0 + i1); //cons(i0+i1, enc_sym(DEF_REPR_BOXED_I_TYPE));
    break;
  case PTR_TYPE_BOXED_F:
    f0 = dec_f(t_max);
    f1 = as_f(t_min);
    f0 = f0 + f1;
    //memcpy(&retval, &f0, sizeof(FLOAT));
    retval = enc_F(f0); //cons(retval, enc_sym(DEF_REPR_BOXED_F_TYPE));
    break;
  }
  return retval;
}

static UINT mul2(UINT a, UINT b) {

  UINT retval = enc_sym(symrepr_terror());
  UINT t_min;
  UINT t_max;
  INT i0;
  INT i1;
  UINT u0;
  UINT u1;
  FLOAT f0;
  FLOAT f1;

  if (type_of(a) < type_of(b)) {
    t_min = a;
    t_max = b;
  } else {
    t_min = b;
    t_max = a;
  }

  if (!is_number(t_min)) enc_sym(symrepr_nil());
 
  switch (type_of(t_max)) {
  case VAL_TYPE_I:
    i0 = dec_i(t_max);
    i1 = as_i(t_min);
    retval = enc_i(i0 * i1);
    break;
  case VAL_TYPE_U:
    u0 = dec_u(t_max);
    u1 = as_u(t_min);
    retval = enc_u(u0 * u1);
    break;
  case PTR_TYPE_BOXED_U:
    u0 = dec_U(t_max);
    u1 = as_u(t_min);
    retval = enc_U(u0 * u1); //cons(u0+u1, enc_sym(DEF_REPR_BOXED_U_TYPE));
    break;
  case PTR_TYPE_BOXED_I:
    i0 = dec_I(t_max);
    i1 = as_u(t_min);
    retval = enc_I(i0 * i1); //cons(i0+i1, enc_sym(DEF_REPR_BOXED_I_TYPE));
    break;
  case PTR_TYPE_BOXED_F:
    f0 = dec_f(t_max);
    f1 = as_f(t_min);
    f0 = f0 * f1;
    //memcpy(&retval, &f0, sizeof(FLOAT));
    retval = enc_F(f0); //cons(retval, enc_sym(DEF_REPR_BOXED_F_TYPE));
    break;
  }
  return retval;
}

static UINT negate(UINT a) {

  UINT retval = enc_sym(symrepr_terror());
  INT i0;
  UINT u0;
  FLOAT f0;

  if (type_of(a) > VAL_TYPE_CHAR) {
    switch (type_of(a)) {
    case VAL_TYPE_I:
      i0 = dec_i(a);
      retval = enc_i(-i0);
      break;
    case VAL_TYPE_U:
      u0 = dec_u(a);
      retval = enc_u(-u0);
      break;
    case PTR_TYPE_BOXED_U:
      u0 = dec_U(a);
      retval = enc_U(-u0); //cons(-u0, enc_sym(DEF_REPR_BOXED_U_TYPE));
      break;
    case PTR_TYPE_BOXED_I:
      i0 = dec_I(a);
      retval = enc_I(-i0); //cons(-i0, enc_sym(DEF_REPR_BOXED_I_TYPE));
      break;
    case PTR_TYPE_BOXED_F:
      f0 = dec_f(a);
      f0 = -f0;
      //memcpy(&retval, &f0, sizeof(FLOAT));
      retval = enc_F(f0); //cons(retval, enc_sym(DEF_REPR_BOXED_F_TYPE));
      break;
    }
  }
  return retval;
}

static UINT sub2(UINT a, UINT b) {

  UINT retval = enc_sym(symrepr_terror());
  UINT t_min;
  UINT t_max;
  INT i0;
  INT i1;
  UINT u0;
  UINT u1;
  FLOAT f0;
  FLOAT f1;

  if (type_of(a) < type_of(b)) {
    t_min = a;
    t_max = b;
  } else {
    t_min = b;
    t_max = a;
  }

  if (type_of(t_min) > VAL_TYPE_CHAR) {
    switch (type_of(t_max)) {
    case VAL_TYPE_I:
      i0 = dec_i(t_max);
      i1 = as_i(t_min);
      retval = enc_i(i0 - i1);
      break;
    case VAL_TYPE_U:
      u0 = dec_u(t_max);
      u1 = as_u(t_min);
      retval = enc_u(u0 - u1);
      break;
    case PTR_TYPE_BOXED_U:
      u0 = dec_U(t_max);
      u1 = as_u(t_min);
      retval = enc_U(u0 - u1);
      break;
    case PTR_TYPE_BOXED_I:
      i0 = dec_I(t_max);
      i1 = as_u(t_min);
      retval = enc_I(i0 - i1);
      break;
    case PTR_TYPE_BOXED_F:
      f0 = dec_f(t_max);
      f1 = as_f(t_min);
      f0 = f0 - f1;
      retval = enc_F(f0);
      break;
    }
  }
  return retval;
}

static bool array_equality(VALUE a, VALUE b) {
  if (type_of(a) == PTR_TYPE_ARRAY &&
      type_of(a) == type_of(b)) {
    array_t *a_ = (array_t*)car(a);
    array_t *b_ = (array_t*)car(b);

    if (a_->elt_type == b_->elt_type &&
	a_->size == b_->size) {
      switch(a_->elt_type) {
      case VAL_TYPE_U:
      case PTR_TYPE_BOXED_U:
	if (memcmp(a_->data.u, b_->data.u, a_->size * sizeof(UINT)) == 0) return true;
	break;
      case VAL_TYPE_I:
      case PTR_TYPE_BOXED_I:
	if (memcmp(a_->data.i, b_->data.i, a_->size * sizeof(INT)) == 0) return true;
	break;
      case VAL_TYPE_CHAR:
	if (memcmp(a_->data.c, b_->data.c, a_->size) == 0) return true;
	break;
      case PTR_TYPE_BOXED_F:
	if (memcmp(a_->data.f, b_->data.f, a_->size * sizeof(FLOAT)) == 0) return true;
	break;
      default:
	break; 
      }
    }
  }
  return false; 
}

static bool struct_eq(VALUE a, VALUE b) {

  if (!is_ptr(a) && !is_ptr(b)) {
    if (val_type(a) == val_type(b)){
      switch (val_type(a)) {
      case VAL_TYPE_SYMBOL:
	return (dec_sym(a) == dec_sym(b));
      case VAL_TYPE_I:
	return (dec_i(a) == dec_i(b));
      case VAL_TYPE_U:
	return (dec_u(a) == dec_u(b));
      case VAL_TYPE_CHAR:
	return (dec_char(a) == dec_char(b));
      default:
	return false;
	break;
      }
    } else {
      return false;
    }
  }

  if (is_ptr(a) && is_ptr(b)) {
    if (ptr_type(a) == ptr_type(b)) {
      switch (ptr_type(a)) {
      case PTR_TYPE_CONS:
	return ( struct_eq(car(a),car(b)) &&
		 struct_eq(cdr(a),cdr(b)) );
      case PTR_TYPE_BOXED_I:
	return ((INT)car(a) == (INT)car(b));
      case PTR_TYPE_BOXED_U:
	return (car(a) == car(b));
      case PTR_TYPE_BOXED_F:
	return ((FLOAT)car(a) == (FLOAT)car(b));
      case PTR_TYPE_ARRAY:
	return array_equality(a, b);
      default:
	return false;
      }
    }
  }
  return false;
}

static int cmpi(INT a, INT b, bool swapped) {
  int res = (a > b) - (a < b);
  return swapped ? -res : res;
}

static int cmpu(UINT a, UINT b, bool swapped) {
  int res = (a > b) - (a < b);
  return swapped ? -res : res;
}

static int cmpf(FLOAT a, FLOAT b, bool swapped) {
  int res = (a > b) - (a < b);
  return swapped ? -res : res;
}


/* returns -1 if a < b; 0 if a = b; 1 if a > b */
static int compare(UINT a, UINT b) {
  int retval = 1;
  UINT tmp;
  INT i0;
  INT i1;
  UINT u0;
  UINT u1;
  FLOAT f0;
  FLOAT f1;
  bool swapped = false;

  if (type_of(a) < type_of(b)) {
    tmp = a;
    a   = b;
    b   = tmp;
    swapped = true;
  }

  if (type_of(b) > VAL_TYPE_CHAR) {
    switch (type_of(a)) {
    case VAL_TYPE_I:
      i0 = dec_i(a);
      i1 = as_i(b);
      retval = cmpi(i0,i1,swapped);
      break;
    case VAL_TYPE_U:
      u0 = dec_u(a);
      u1 = as_u(b);
      retval = cmpu(u0,u1,swapped);
      break;
    case PTR_TYPE_BOXED_U:
      u0 = dec_U(a);
      u1 = as_u(b);
      retval = cmpu(u0,u1,swapped);
      break;
    case PTR_TYPE_BOXED_I:
      i0 = dec_I(a);
      i1 = as_u(b);
      retval = cmpi(i0,i1,swapped);
      break;
    case PTR_TYPE_BOXED_F:
      f0 = dec_f(a);
      f1 = as_f(b);
      retval = cmpf(f0,f1,swapped);
      break;
    }
  }
  return retval;
}


void array_read(VALUE *args, UINT nargs, UINT *result) {
  (void) nargs;
  // Args are: array, index
  VALUE arr = args[0];
  VALUE index = args[1];

  // Get array index
  UINT ix;
  INT  tmp;

  *result = enc_sym(symrepr_eerror());
  switch (type_of(index)) {
  case VAL_TYPE_U:
    ix = dec_u(index);
    break;
  case VAL_TYPE_I:
    tmp = (INT)dec_i(index);
    if (tmp < 0) {
      *result = enc_sym(symrepr_eerror());
      return;
    }
    ix = (UINT)tmp;
    break;
  case PTR_TYPE_BOXED_U:
    ix = dec_U(index);
    break;
  case PTR_TYPE_BOXED_I:
    tmp = dec_I(index);
    if (tmp < 0) {
      *result = enc_sym(symrepr_eerror());
      return;
    }
    ix = (UINT) tmp;
    break;
  default:
    *result = enc_sym(symrepr_nil());
    return;
  }

  if (type_of(arr) == PTR_TYPE_ARRAY) {
    array_t *array = (array_t*)car(arr);

    if (ix >= array->size){
      *result = enc_sym(symrepr_nil());
      return;
    }

    switch(array->elt_type) {
    case VAL_TYPE_CHAR:
      *result = enc_char((UINT) array->data.c[ix]);
      break;
    case VAL_TYPE_U:
      *result = enc_u(array->data.u[ix]);
      break;
    case VAL_TYPE_I:
      *result = enc_i(array->data.i[ix]);
      break;
    case PTR_TYPE_BOXED_U:
      *result = cons(array->data.u[ix], enc_sym(DEF_REPR_BOXED_U_TYPE));
      if (type_of(*result) == VAL_TYPE_SYMBOL) return;
      *result = set_ptr_type(*result, PTR_TYPE_BOXED_U);
      break;
    case PTR_TYPE_BOXED_I:
      *result = cons(array->data.i[ix], enc_sym(DEF_REPR_BOXED_I_TYPE));
      if (type_of(*result) == VAL_TYPE_SYMBOL) return;
      *result = set_ptr_type(*result, PTR_TYPE_BOXED_I);
      break;
    case PTR_TYPE_BOXED_F:
      *result = cons(array->data.f[ix], enc_sym(DEF_REPR_BOXED_F_TYPE));
      if (type_of(*result) == VAL_TYPE_SYMBOL) return;
      *result = set_ptr_type(*result, PTR_TYPE_BOXED_F);
      break;
    default:
      *result = enc_sym(symrepr_eerror());
      return;
    }
    return;
  }
  *result = enc_sym(symrepr_eerror());
}

void array_write(VALUE *args, UINT nargs, UINT *result) {
  (void) nargs;
  VALUE arr = args[0];
  VALUE index = args[1];
  VALUE val = args[2];
  UINT uv;
  FLOAT v;
  UINT ix;
  INT tmp;
  switch (type_of(index)) {
  case VAL_TYPE_U:
    ix = dec_u(index);
    break;
  case VAL_TYPE_I:
    tmp = (INT)dec_i(index);
    if (tmp < 0) {
      *result =  enc_sym(symrepr_eerror());
      return;
    }
    ix = (UINT) tmp;
    break;
  case PTR_TYPE_BOXED_U:
    ix = car(index);
    break;
  case PTR_TYPE_BOXED_I:
    tmp = (INT)car(index);
    if (tmp < 0) {
      *result = enc_sym(symrepr_eerror());
      return;
    }
    ix = (UINT) tmp;
    break;
  default:
    *result = enc_sym(symrepr_nil());
    return;
  }

  if (type_of(arr) == PTR_TYPE_ARRAY) {
    array_t *array = (array_t*)car(arr);

    if (type_of(val) != array->elt_type ||
	ix >= array->size) {
      *result =  enc_sym(symrepr_nil());
      return;
    }

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
      array->data.u[ix] = dec_U(val);
      break;
    case PTR_TYPE_BOXED_I:
      array->data.i[ix] = dec_I(val);
      break;
    case PTR_TYPE_BOXED_F:
      uv = car(val);
      memcpy(&v, &uv, sizeof(FLOAT));
      array->data.f[ix] = v;
      break;
    default:
      *result = enc_sym(symrepr_eerror());
      return;
    }
    *result = enc_sym(symrepr_true());
    return;
  }
  *result = enc_sym(symrepr_nil());
}


void array_create(VALUE *args, UINT nargs, UINT *result) {
  (void) args;
  (void) nargs;
  (void) result;

}

VALUE fundamental_exec(VALUE* args, UINT nargs, VALUE op) {

  UINT result = enc_sym(symrepr_eerror());
  int cmp_res = -1;

  switch (dec_sym(op)) {
  case SYM_EVAL:
    result = eval_cps_bi_eval(args[0]);
    break;
  case SYM_CONS: {
    UINT a = args[0];
    UINT b = args[1];
    result = cons(a,b);
    break;
  }
  case SYM_CAR: {
    result = car(args[0]);
    break;
  }
  case SYM_CDR: {
    result = cdr(args[0]);
    break;
  }
  case SYM_LIST: {
    result = enc_sym(symrepr_nil());
    for (UINT i = 1; i <= nargs; i ++) {
      result = cons(args[nargs-i], result);
      if (type_of(result) == VAL_TYPE_SYMBOL)
	break;
    }
    break;
  }
  case SYM_ADD: {
    UINT sum = args[0];
    for (UINT i = 1; i < nargs; i ++) {
      sum = add2(sum, args[i]);
      if (type_of(sum) == VAL_TYPE_SYMBOL) {
	break;
      }
    }
    result = sum;
    break;
  }
  case SYM_SUB: {
    UINT res = args[0];

    if (nargs == 1) {
      res = negate(res);
    } else {
      for (UINT i = 1; i < nargs; i ++) {
	res = sub2(res, args[i]);
	if (type_of(res) == VAL_TYPE_SYMBOL)
	  break;
      }
    }
    result = res;
    break;
  }
  case SYM_MUL: {
    UINT prod = args[0];
    for (UINT i = 1; i < nargs; i ++) {
      prod = mul2(prod, args[i]);
      if (type_of(prod) == VAL_TYPE_SYMBOL) {
	break;
      }
    }
    result = prod;
    break;
  }
  case SYM_DIV:
    break;
  case SYM_MOD:
    break;
  case SYM_EQ: {
    UINT a = args[0];
    UINT b;
    bool r = true;

    for (UINT i = 1; i < nargs; i ++) {
      b = args[i];
      r = r && struct_eq(a, b);
    }
    if (r) {
      result = enc_sym(symrepr_true());
    } else {
      result = enc_sym(symrepr_nil());
    }
    break;
  }
  case SYM_NUMEQ:
    cmp_res = 0;
    /* fall through */
  case SYM_GT:
    if (dec_sym(op) == SYM_GT) cmp_res = 1;
    /* fall through */
  case SYM_LT: {
    UINT a = args[0];
    UINT b;
    bool r = true;
    bool ok = true;

    if (!is_number(a)) {
      result = enc_sym(symrepr_terror());
      break;
    }
    for (UINT i = 1; i < nargs; i ++) {
      b = args[i];
      if (!is_number(b)) {
	ok = false;
	break;
      }
      r = r && (compare(a, b) == cmp_res);
    }
    if (ok) {
      if (r) {
	result = enc_sym(symrepr_true());
      } else {
	result = enc_sym(symrepr_nil());
      }
    } else {
      result = enc_sym(symrepr_terror());
    }
    break;
  }

  case SYM_ARRAY_READ:
    array_read(args, nargs, &result);
    break;
  case SYM_ARRAY_WRITE:
    array_write(args, nargs, &result);
    break;
  case SYM_ARRAY_CREATE:
    array_create(args, nargs, &result);
    break;
  default:
    result = enc_sym(symrepr_eerror());
    break;
  }

  return result;
}
