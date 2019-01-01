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

#include "symrepr.h"
#include "heap.h"
#include "builtin.h"
#include "print.h"

typedef struct s_builtin_function{
  uint32_t sym;
  bi_fptr fun_ptr;
  struct s_builtin_function* next; 
} builtin_function_t; 

builtin_function_t* function_list = NULL;

// Built in functions

uint32_t bi_fun_car(uint32_t args) {
  return car(car(args));
}

uint32_t bi_fun_cdr(uint32_t args) {
  return cdr(car(args)); 
}

uint32_t bi_fun_cons(uint32_t args) {
  uint32_t a = car(args);
  uint32_t b = car(cdr(args));
  return cons(a,b);
}

uint32_t bi_fun_reverse(uint32_t args) {
  uint32_t xs = car(args);
  return reverse(xs); 
}

uint32_t get_maximum_type(uint32_t args) {

  uint32_t max_type = 0; 
  uint32_t curr = args;

  while ( TYPE_OF(curr) == PTR_TYPE_CONS ) {

    if (TYPE_OF(car(curr)) > max_type) {
      max_type = TYPE_OF(car(curr));
    }
    curr = cdr(curr);
  }
  return max_type;
}

int get_as_int(uint32_t v, int32_t *r) {
  uint32_t tmp;
  float f_tmp;
  switch (TYPE_OF(v)) {
  case VAL_TYPE_I28:
    *r =  DEC_I28(v);
    break;
  case VAL_TYPE_U28:
    *r = (int32_t)DEC_U28(v);
    break;
  case PTR_TYPE_I32:
    *r = (int32_t)car(v);
    break;
  case PTR_TYPE_U32:
    *r = (int32_t)car(v);
    break;
  case PTR_TYPE_F32:
    tmp = car(v);
    f_tmp = *(float*)&tmp;
    *r = (int32_t)f_tmp;
    break;
  default:
    return 0;
  }
  return 1;
}

int get_as_uint(uint32_t v, uint32_t *r) {
  uint32_t tmp;
  float    f_tmp;
  switch (TYPE_OF(v)) {
  case VAL_TYPE_I28:
    *r =  (uint32_t)DEC_I28(v);
    break;
  case VAL_TYPE_U28:
    *r = DEC_U28(v);
    break;
  case PTR_TYPE_I32:
    *r = (uint32_t)car(v); 
  case PTR_TYPE_U32:
    *r = car(v);
    break;
  case PTR_TYPE_F32:
    tmp = car(v);
    f_tmp = *(float*)&tmp;
    *r = (uint32_t)f_tmp;
    break;
  default:
    return 0;
  }
  return 1;
}

int get_as_float(uint32_t v, float *r) {
  uint32_t tmp;
  switch (TYPE_OF(v)) {
  case VAL_TYPE_I28:
    *r =  (float)DEC_I28(v);
    break;
  case VAL_TYPE_U28:
    *r = (float)DEC_U28(v);
    break;
  case PTR_TYPE_I32:
    *r = (float)((int32_t)car(v));
    break;
  case PTR_TYPE_U32:
    *r = (float)car(v);
    break;
  case PTR_TYPE_F32:
    tmp = car(v);
    *r = *(float*)&tmp;
    break;
  default:
    return 0;
  }
  return 1;
}

uint32_t bi_fun_sum(uint32_t args) { 
  uint32_t curr = args;
  int32_t  i_sum = 0;
  uint32_t u_sum = 0;
  float    f_sum = 0.0; 

  uint32_t tmp;
  uint32_t float_enc;
  
  uint32_t max_type = get_maximum_type(args);

  switch (max_type) {

  case VAL_TYPE_I28:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      int32_t r;
      get_as_int(v,&r);
      i_sum += r;
      curr = cdr(curr);
    }
    return ENC_I28(i_sum);

  case VAL_TYPE_U28:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      uint32_t r; 
      get_as_uint(v,&r);
      u_sum += r;
      curr = cdr(curr); 
    }
    return ENC_U28(u_sum);

  case PTR_TYPE_I32:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      uint32_t r;
      get_as_int(v,&r);
      i_sum += r;
      curr= cdr(curr);
    }
    tmp = cons(i_sum,symrepr_nil());
    return SET_PTR_TYPE(tmp, PTR_TYPE_I32);
    
  case PTR_TYPE_U32:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      uint32_t r;
      get_as_uint(v,&r);
      u_sum += r;
      curr= cdr(curr);
    }
    tmp = cons(u_sum,symrepr_nil());
    return SET_PTR_TYPE(tmp, PTR_TYPE_U32);
	 
  case PTR_TYPE_F32:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      float r; 
      get_as_float(v,&r);
      f_sum += r;
      curr = cdr(curr); 
    }
    tmp = *(uint32_t*)&f_sum;
    float_enc = cons(tmp,ENC_SYM(symrepr_nil()));
    float_enc = SET_PTR_TYPE(float_enc, PTR_TYPE_F32);
    return float_enc;
  }

  return ENC_SYM(symrepr_eerror());
}

uint32_t bi_fun_sub(uint32_t args) { 

  uint32_t tmp;
  uint32_t enc;
  float f_res;
  uint32_t u_res;
  int32_t i_res; 
  uint32_t max_type = get_maximum_type(args);

  uint32_t n = length(args);

  if (n < 1) return ENC_SYM(symrepr_eerror());
  
  // A negation
  if (n == 1) {
    switch (max_type) {

    case VAL_TYPE_I28:
      i_res = -DEC_I28(car(args));
      return ENC_I28(i_res);
     
    case VAL_TYPE_U28:
      u_res = -DEC_U28(car(args));
      return ENC_U28(u_res);

    case PTR_TYPE_I32:
      i_res = -(int32_t)car(car(args));
      enc = cons(i_res, ENC_SYM(symrepr_nil()));
      enc = SET_PTR_TYPE(enc,PTR_TYPE_I32);
      return enc;

    case PTR_TYPE_U32:
      u_res = -car(car(args));
      enc = cons(u_res, ENC_SYM(symrepr_nil()));
      enc = SET_PTR_TYPE(enc,PTR_TYPE_U32);
      return enc;
      
    case PTR_TYPE_F32:
      enc = car(car(args));
      f_res = -(*(float*)&enc);
      tmp = *(uint32_t*)&f_res;
      enc = cons(tmp,ENC_SYM(symrepr_nil()));
      enc = SET_PTR_TYPE(enc, PTR_TYPE_F32);
    return enc;
      break;
    }
  }

  uint32_t v = car(args);
  uint32_t curr = cdr(args);
  
  switch (max_type) {
  case VAL_TYPE_I28:
    get_as_int(v, &i_res);

    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      int32_t r;
      get_as_int(v,&r);
      i_res -= r;
      curr = cdr(curr); 
    }
    return ENC_I28(i_res);
    
  case VAL_TYPE_U28:
    get_as_uint(v, &u_res);
    
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      uint32_t r;
      get_as_uint(v,&r);
      u_res -= r;
      curr = cdr(curr); 
    }
    return ENC_U28(u_res);

  case PTR_TYPE_I32:
    get_as_int(v, &u_res);

    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      uint32_t r;
      get_as_int(v, &r);
      i_res -= r;
      curr = cdr(curr);
    }
    enc = cons(i_res, ENC_SYM(symrepr_nil()));
    enc = SET_PTR_TYPE(enc,PTR_TYPE_I32);
    return enc;
    
  case PTR_TYPE_U32:
    get_as_uint(v, &u_res);

    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      uint32_t r;
      get_as_uint(v, &r);
      u_res -= r;
      curr = cdr(curr);
    }
    enc = cons(u_res, ENC_SYM(symrepr_nil()));
    enc = SET_PTR_TYPE(enc,PTR_TYPE_U32);
    return enc;
    
  case PTR_TYPE_F32:
    get_as_float(v, &f_res);

    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      float r;
      get_as_float(v, &r);
      f_res -= r;
      curr = cdr(curr); 
    }
    tmp = *(uint32_t*)&f_res;
    enc = cons(tmp,ENC_SYM(symrepr_nil()));
    enc = SET_PTR_TYPE(enc, PTR_TYPE_F32);
    return enc;
  }

  return ENC_SYM(symrepr_eerror());
}


uint32_t bi_fun_gt(uint32_t args) {
  uint32_t a1 = car(args);
  uint32_t a2 = car(cdr(args));
  int32_t i1, i2;
  uint32_t u1, u2;
  float f1, f2;
  
  uint32_t max_type = (TYPE_OF(a1) >= TYPE_OF(a2)) ? TYPE_OF(a1) : TYPE_OF(a2);

  switch (max_type) {

  case VAL_TYPE_I28:
    get_as_int(a1, &i1);
    get_as_int(a2, &i2);
    if (i1 > i2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  case VAL_TYPE_U28:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 > u2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  case PTR_TYPE_I32:
    get_as_int(a1, &i1);
    get_as_int(a2, &i2);
    if (i1 > i2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  case PTR_TYPE_U32:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 > u2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  case PTR_TYPE_F32:
    get_as_float(a1, &f1);
    get_as_float(a2, &f2);
    if (f1 > f2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  }

  return ENC_SYM(symrepr_eerror());
}

uint32_t bi_fun_lt(uint32_t args) {
  uint32_t a1 = car(args);
  uint32_t a2 = car(cdr(args));
  int32_t i1, i2;
  uint32_t u1, u2;
  float f1, f2;
  
  uint32_t max_type = (TYPE_OF(a1) >= TYPE_OF(a2)) ? TYPE_OF(a1) : TYPE_OF(a2);

  switch (max_type) {

  case VAL_TYPE_I28:
    get_as_int(a1, &i1);
    get_as_int(a2, &i2);
    if (i1 < i2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  case VAL_TYPE_U28:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 < u2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  case PTR_TYPE_I32:
    get_as_uint(a1, &i1);
    get_as_uint(a2, &i2);
    if (i1 < i2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  case PTR_TYPE_U32:
    get_as_uint(a1, &u1);
    get_as_uint(a2, &u2);
    if (u1 < u2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  case PTR_TYPE_F32:
    get_as_float(a1, &f1);
    get_as_float(a2, &f2);
    if (f1 < f2) return ENC_SYM(symrepr_true());
    else return ENC_SYM(symrepr_nil());
  }

  return ENC_SYM(symrepr_eerror());
}

int structural_equality(uint32_t a, uint32_t b) {
  
  if (!IS_PTR(a) && !IS_PTR(b)) {
    if (VAL_TYPE(a) == VAL_TYPE(b)){
      switch (VAL_TYPE(a)) {
      case VAL_TYPE_SYMBOL:
	if (DEC_SYM(a) == DEC_SYM(b)) return 1;
        else return 0;
	break;
      case VAL_TYPE_I28:
	if (DEC_I28(a) == DEC_I28(b)) return 1;
	else return 0;
	break;
      case VAL_TYPE_U28:
	if (DEC_U28(a) == DEC_U28(b)) return 1;
	else return 0;
	break;
      case VAL_TYPE_CHAR:
	if (DEC_CHAR(a) == DEC_CHAR(b)) return 1;
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
  
  if (IS_PTR(a) && IS_PTR(b)) {
    if (PTR_TYPE(a) == PTR_TYPE(b)) {
      if ( PTR_TYPE(a) == PTR_TYPE_CONS ) {
	int car_eq = structural_equality(car(a),car(b));
	int cdr_eq = structural_equality(cdr(a),cdr(b));
	if ( car_eq && cdr_eq ) return 1;
	else return 0;
      }

      if (PTR_TYPE(a) == PTR_TYPE_I32){
	int32_t ai = (int32_t)car(a);
	int32_t bi = (int32_t)car(b);
	  if (ai == bi) return 1;
	  else return 0; 
      }
      
      if (PTR_TYPE(a) == PTR_TYPE_U32){
	uint32_t au = car(a);
	uint32_t bu = car(b);
	  if (au == bu) return 1;
	  else return 0; 
      }

      if (PTR_TYPE(a) == PTR_TYPE_F32) {
	float af = car(a);
	float bf = car(b);
	  if (af == bf) return 1;
	  else return 0; 
      }
      printf("TODO: Structural equality for this ptr type not implemented\n");
    } else {
      return 0;
    }
  }

  return 0; 
}

uint32_t bi_fun_eq(uint32_t args) {
  uint32_t a1 = car(args);
  uint32_t a2 = car(cdr(args));
  
  return( structural_equality(a1, a2) ? ENC_SYM(symrepr_true()) : ENC_SYM(symrepr_nil()) );
}

uint32_t bi_fun_gensym(uint32_t args) {

  // Takes no arguments!
  uint32_t gs;
  int res = gensym(&gs);

  if (res) return ENC_SYM(gs);
  return ENC_SYM(symrepr_eerror()); 
}

uint32_t bi_fun_list(uint32_t args) {
  uint32_t t = ENC_SYM(symrepr_nil());
  uint32_t list = ENC_SYM(symrepr_nil());
  uint32_t curr = args; 
  while (IS_PTR(curr) && PTR_TYPE(curr) == PTR_TYPE_CONS) {
    t = cons(car(curr),t);
    
    curr = cdr(curr); 
  }
  curr = t; 
  while (IS_PTR(curr) && PTR_TYPE(curr) == PTR_TYPE_CONS) {
    list = cons(car(curr),list);

    curr = cdr(curr);
  }
  return list;
}

// Built in array functions

uint32_t bi_fun_array_read(uint32_t args) {
  // Args are: array, index
  uint32_t arr = car(args);
  uint32_t index = car(cdr(args));

  // Get array index
  int32_t ix;
  uint32_t res;
  switch (TYPE_OF(index)) {
  case VAL_TYPE_U28:
    ix = (int32_t)DEC_U28(index);
    break;
  case VAL_TYPE_I28:
    ix = (int32_t)DEC_I28(index);
    break;
  case PTR_TYPE_U32:
    ix = (int32_t)car(index);
    break;
  case PTR_TYPE_I32:
    ix = (int32_t)car(index);
    break;
  default:
    return ENC_SYM(symrepr_nil()); 
  }

  if (TYPE_OF(arr) == PTR_TYPE_ARRAY) {
    array_t *array = (array_t*)car(arr);

    if (ix < 0 || ix >= array->size) return ENC_SYM(symrepr_nil()); 
    
    switch(array->elt_type) {
    case VAL_TYPE_CHAR:
      res = ENC_CHAR((uint32_t) array->data.c[ix]);
      break;
    case VAL_TYPE_U28:
      res = ENC_U28(array->data.u32[ix]);
      break;
    case VAL_TYPE_I28:
      res = ENC_I28(array->data.i32[ix]);
      break;
    case PTR_TYPE_U32:
      res = cons(array->data.u32[ix], ENC_SYM(symrepr_nil()));
      res = SET_PTR_TYPE(res, PTR_TYPE_U32);
      break;
    case PTR_TYPE_I32:
      res = cons(array->data.i32[ix], ENC_SYM(symrepr_nil()));
      res = SET_PTR_TYPE(res, PTR_TYPE_I32);
      break;
    case PTR_TYPE_F32:
      res = cons(array->data.f[ix], ENC_SYM(symrepr_nil()));
      res = SET_PTR_TYPE(res, PTR_TYPE_F32);
      break;
    default:
      printf("unknown type!\n");
      return ENC_SYM(symrepr_eerror);
    }
  }
  return res; 
}

uint32_t bi_fun_array_write(uint32_t args) {
  // Args are: array, index, value
  uint32_t arr = car(args);
  uint32_t index = car(cdr(args));
  uint32_t val = car(cdr(cdr(args)));

  
  
}


// Interface functions

bi_fptr builtin_lookup_function(uint32_t sym){
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

  uint32_t symbol;
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
  res &= builtin_add_function("gensym", bi_fun_gensym);
  res &= builtin_add_function("list", bi_fun_list);
  res &= builtin_add_function("reverse", bi_fun_reverse);
  res &= builtin_add_function("array-read", bi_fun_array_read);
  res &= builtin_add_function("array-write", bi_fun_array_write); 
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
}

uint32_t built_in_gen_env(void) {

  builtin_function_t* curr = function_list;

  uint32_t env = ENC_SYM(symrepr_nil()); 
  
  while (curr) {
    uint32_t sym = ENC_SYM(curr->sym);
    env = cons(cons(sym,sym),env);
    curr = curr->next;
  }

  return env;   
}
