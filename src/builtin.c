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
      switch (TYPE_OF(v)) {
      case VAL_TYPE_I28:
	i_sum += DEC_I28(v);
	break;
      default:
	return ENC_SYM(symrepr_eerror());
	break;
      }
      curr = cdr(curr); 
    }
    break; 
  case VAL_TYPE_U28:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      switch (TYPE_OF(v)) {
      case VAL_TYPE_I28:
	u_sum += (uint32_t)DEC_I28(v);
	break;
      case VAL_TYPE_U28:
	u_sum += DEC_U28(v);
	break;
      default:
	return ENC_SYM(symrepr_eerror());
      }
      curr = cdr(curr); 
    }
    break;
  case PTR_TYPE_F32:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      switch (TYPE_OF(v)) {
      case VAL_TYPE_I28:
	f_sum += (float)DEC_I28(v);
	break;
      case VAL_TYPE_U28:
	f_sum += (float)DEC_U28(v);
	break;
      case PTR_TYPE_F32:
	tmp = car(v);
	f_sum += *(float*)&tmp;
	break;
      default:
	return ENC_SYM(symrepr_eerror());
      }
      curr = cdr(curr); 
    }
    break;
  default:
    return ENC_SYM(symrepr_eerror());
  }

  switch (max_type) {
  case VAL_TYPE_I28:
    return ENC_I28(i_sum);
  case VAL_TYPE_U28:
    return ENC_U28(u_sum);
  case PTR_TYPE_F32:
    tmp = *(uint32_t*)&f_sum;
    float_enc = cons(tmp,ENC_SYM(symrepr_nil()));
    float_enc = SET_PTR_TYPE(float_enc, PTR_TYPE_F32);
    return float_enc;
  }
  return ENC_SYM(symrepr_eerror());
}

uint32_t bi_fun_sub(uint32_t args) { 
  uint32_t curr = cdr(args);

  uint32_t tmp;
  uint32_t float_enc;
  float f_res;
  uint32_t u_res;
  int32_t i_res; 
  uint32_t max_type = get_maximum_type(args);

  uint32_t n = length(args);
  
  if (n == 1) {
    switch (max_type) {
    case VAL_TYPE_I28:
      i_res = -DEC_I28(car(args));
      break;
    case VAL_TYPE_U28:
      u_res = -DEC_U28(car(args));
    case PTR_TYPE_F32:
      float_enc = car(car(args));
      f_res = -(*(float*)&float_enc);
      break;
    }
  } else {
    switch (max_type) {
    case VAL_TYPE_I28:
      i_res = DEC_I28(car(args));
      break;
    case VAL_TYPE_U28:
      switch (TYPE_OF(car(args))) {
      case VAL_TYPE_I28:
	u_res = (uint32_t)DEC_I28(car(args));
	break;
      case VAL_TYPE_U28:
	u_res = DEC_U28(car(args));
	break;
      default:
	return ENC_SYM(symrepr_eerror());
      }
      break;
    case PTR_TYPE_F32:
      switch (TYPE_OF(car(args))) {
      case VAL_TYPE_I28:
	f_res = (float)DEC_I28(car(args));
	break;
      case VAL_TYPE_U28:
	f_res = (float) DEC_U28(car(args));
	break;
      case PTR_TYPE_F32: 
	float_enc = car(car(args));
	f_res = (*(float*)&float_enc);
	break;
      default:
	return ENC_SYM(symrepr_eerror());
      }
      break;
    default:
      return ENC_SYM(symrepr_eerror());
    }
  }
  // loop through args 
  switch (max_type) {
  case VAL_TYPE_I28:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      switch (TYPE_OF(v)) {
      case VAL_TYPE_I28:
	i_res -= DEC_I28(v);
	break;
      default:
	return ENC_SYM(symrepr_eerror());
	break;
      }
      curr = cdr(curr); 
    }
    break; 
  case VAL_TYPE_U28:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      switch (TYPE_OF(v)) {
      case VAL_TYPE_I28:
	u_res -= (uint32_t)DEC_I28(v);
	break;
      case VAL_TYPE_U28:
	u_res -= DEC_U28(v);
	break;
      default:
	return ENC_SYM(symrepr_eerror());
      }
      curr = cdr(curr); 
    }
    break;
  case PTR_TYPE_F32:
    while(TYPE_OF(curr) == PTR_TYPE_CONS) {
      uint32_t v = car(curr);
      switch (TYPE_OF(v)) {
      case VAL_TYPE_I28:
	f_res -= (float)DEC_I28(v);
	break;
      case VAL_TYPE_U28:
	f_res -= (float)DEC_U28(v);
	break;
      case PTR_TYPE_F32:
	tmp = car(v);
	f_res -= *(float*)&tmp;
	break;
      default:
	return ENC_SYM(symrepr_eerror());
      }
      curr = cdr(curr); 
    }
    break;
  default:
    return ENC_SYM(symrepr_eerror());
  }

  // Return result
  switch (max_type) {
  case VAL_TYPE_I28:
    return ENC_I28(i_res);
  case VAL_TYPE_U28:
    return ENC_U28(u_res);
  case PTR_TYPE_F32:
    tmp = *(uint32_t*)&f_res;
    float_enc = cons(tmp,ENC_SYM(symrepr_nil()));
    float_enc = SET_PTR_TYPE(float_enc, PTR_TYPE_F32);
    return float_enc;
  }
  return ENC_SYM(symrepr_eerror());
}


uint32_t bi_fun_gt(uint32_t args) {
  uint32_t a1 = car(args);
  uint32_t a2 = car(cdr(args));
  
  //TODO: error checking and type promotion
  if (DEC_I28(a1) > DEC_I28(a2)) {
    return ENC_SYM(symrepr_true());
  }
  return ENC_SYM(symrepr_nil());
}

uint32_t bi_fun_lt(uint32_t args) {
  uint32_t a1 = car(args);
  uint32_t a2 = car(cdr(args));

  //TODO: error checking and type promotion
  if (DEC_I28(a1) < DEC_I28(a2)) {
    return ENC_SYM(symrepr_true());
  }
  return ENC_SYM(symrepr_nil());
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
      } else {
	printf("TODO: Implement\n");
	return 0; 
      }
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
