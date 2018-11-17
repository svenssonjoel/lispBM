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

uint32_t bi_fun_sum(uint32_t args) { // TODO: typechecking and potential conversion
  uint32_t tmp = args;
  int32_t sum = 0;
  while ( DEC_SYM(tmp) != symrepr_nil()) {
    int32_t v = car(tmp);
    sum += DEC_I28(v);
    tmp = cdr(tmp); 
  }
  return ENC_I28(sum);
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
  res &= builtin_add_function("car", bi_fun_car);
  res &= builtin_add_function("cdr", bi_fun_cdr); 
  res &= builtin_add_function("cons", bi_fun_cons); 
  res &= builtin_add_function(">" , bi_fun_gt);
  res &= builtin_add_function("<" , bi_fun_lt); 
  
  return res; 
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
