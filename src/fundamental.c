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

static inline UINT as_i(UINT a) {
  
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

static inline UINT as_u(UINT a) {
  
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

static inline UINT as_f(UINT a) {
  
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



static inline UINT add2(UINT a, UINT b) {

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
      retval = cons(u0+u1, enc_sym(DEF_REPR_BOXED_U_TYPE));
      break;
    case PTR_TYPE_BOXED_I:
      i0 = dec_I(t_max);
      i1 = as_u(t_min);
      retval = cons(i0+i1, enc_sym(DEF_REPR_BOXED_I_TYPE));
      break;
    case PTR_TYPE_BOXED_F:
      f0 = dec_f(t_max);
      f1 = as_f(t_min);
      f0 = f0 + f1;
      memcpy(&retval, &f0, sizeof(FLOAT));
      retval = cons(retval, enc_sym(DEF_REPR_BOXED_F_TYPE));
      break;
    }
  } 
  return retval;
}

static inline UINT negate(UINT a) {

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
      retval = cons(-u0, enc_sym(DEF_REPR_BOXED_U_TYPE));
      break;
    case PTR_TYPE_BOXED_I:
      i0 = dec_I(a);
      retval = cons(-i0, enc_sym(DEF_REPR_BOXED_I_TYPE));
      break;
    case PTR_TYPE_BOXED_F:
      f0 = dec_f(a);
      f0 = -f0;
      memcpy(&retval, &f0, sizeof(FLOAT));
      retval = cons(retval, enc_sym(DEF_REPR_BOXED_F_TYPE));
      break;
    }
  } 
  return retval;
}

static inline UINT sub2(UINT a, UINT b) {

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
      retval = cons(u0+u1, enc_sym(DEF_REPR_BOXED_U_TYPE));
      break;
    case PTR_TYPE_BOXED_I:
      i0 = dec_I(t_max);
      i1 = as_u(t_min);
      retval = cons(i0 - i1, enc_sym(DEF_REPR_BOXED_I_TYPE));
      break;
    case PTR_TYPE_BOXED_F:
      f0 = dec_f(t_max);
      f1 = as_f(t_min);
      f0 = f0 - f1;
      memcpy(&retval, &f0, sizeof(FLOAT));
      retval = cons(retval, enc_sym(DEF_REPR_BOXED_F_TYPE));
      break;
    }
  } 
  return retval;
}


static inline bool struct_eq(VALUE a, VALUE b) {

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
	return false; //array_equality(a, b); 
      default:
	printf("TODO: Structural equality for this ptr type not implemented\n");
	return false;
      }
    }
  }
  return false;
}
 
bool fundamental_exec(stack *K, VALUE op) {
  bool ret = false;
  UINT nargs;
  UINT tmp;
  UINT result = enc_sym(symrepr_eerror());
  pop_u32(K, &nargs);
  /* for now assume that all of these will take at least one argument */
  nargs = dec_u(nargs);
  if (nargs < 1) {
    push_u32(K, symrepr_nil());
  }
  
  switch (dec_sym(op)) {
  case SYM_CONS: {
    UINT a;
    UINT b;
    pop_u32(K, &a); nargs--;
    pop_u32(K, &b); nargs--;
    result = cons(a,b);
    ret = true;
    break;
  }
  case SYM_CAR: {
    pop_u32(K, &tmp); nargs--;
    result = car(tmp);
    ret = true;
    break;
  }
  case SYM_CDR: {
    pop_u32(K, &tmp); nargs--;
    result = cdr(tmp);
    ret = true;
    break;
  }
  case SYM_ADD: {
    UINT sum;
    UINT value;
    pop_u32(K, &sum); nargs--;
    while (nargs > 0) {
      pop_u32(K, &value); nargs--;
      sum = add2(sum, value);
    }
    result = sum;
    ret = true;
    break;
  }
  case SYM_SUB: {
    UINT res;
    UINT value;
    pop_u32(K,&res); nargs--;
    if (nargs == 0) {
      res = negate(res);
    } else {
      while (nargs > 0) {
	pop_u32(K, &value); nargs--;
	res = sub2(res, value);
      }
    }
    result = res;
    ret = true;
    break;
  }
  case SYM_MUL:
    ret = true;
    break;
  case SYM_DIV:
    ret = true;
    break;
  case SYM_MOD:
    ret = true;
    break;
  case SYM_EQ: {
    if (nargs != 2) {
      result = enc_sym(symrepr_eerror());
      while (nargs > 0) {
	UINT scrap;
	pop_u32(K, &scrap); nargs--;
	nargs--;
      }
    } else {
      UINT a;
      UINT b;
      pop_u32(K, &a); nargs--;
      pop_u32(K, &b); nargs--;
      if (struct_eq(a,b)) {
	result = enc_sym(symrepr_true());
      } else {
        result = enc_sym(symrepr_nil());
      }
    }
    ret = true;
    break;
  }
  }

  /* clean up the stack */
  if (nargs > 0) {
    pop_u32(K, &tmp); nargs--;
  }
  
  push_u32(K,result);
  return ret;
}

