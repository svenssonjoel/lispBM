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

bool fundamental_exec(stack *K, VALUE op) {
  bool ret = false;
  UINT nargs;
  pop_u32(K, &nargs);
  /* for now assume that all of these will take at least one argument */
  nargs = dec_u(nargs);
  if (nargs < 1) {
    push_u32(K, symrepr_nil());
  }

  switch (dec_sym(op)) {
  case SYM_ADD: {

    UINT sum;
    UINT value;
    pop_u32(K, &sum); nargs--;
    while (nargs > 0) {
      pop_u32(K, &value);
      sum = add2(sum, value);
      nargs--;
    }
    push_u32(K, sum);
    ret = true;
    break;
  }
  case SYM_SUB:
    ret = true;
    break;
  case SYM_MUL:
    ret = true;
    break;
  case SYM_DIV:
    ret = true;
    break;
  case SYM_MOD:
    ret = true;
    break;
  }
  return ret;
}

