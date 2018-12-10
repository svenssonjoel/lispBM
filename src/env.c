
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


#include "symrepr.h"
#include "heap.h"


// Copies just the skeleton structure of an environment
// The new "copy" will have pointers to the original key-val bindings.
int env_copy_shallow(uint32_t env, uint32_t *cpy) {

  uint32_t res = ENC_SYM(symrepr_nil());
  uint32_t curr = env;
  
  while (TYPE_OF(curr) == PTR_TYPE_CONS) {
    uint32_t key = car(car(curr));
    if (DEC_SYM(key) != symrepr_nil()) {
      res = cons(car(curr), res);

      // Check for "out of memory"
      if (TYPE_OF(res) == VAL_TYPE_SYMBOL &&
	  DEC_SYM(res) == symrepr_nil()) {
	return 0;
      }			      
    }
    curr = cdr(curr);
  }
  *cpy = res; 
  return 1;
}

int env_lookup(uint32_t sym, uint32_t env, uint32_t *res) {
  uint32_t curr = env;
  
  if(DEC_SYM(sym) == symrepr_nil()) {
    *res = ENC_SYM(symrepr_nil());
    return 1;
  }
    
  while (TYPE_OF(curr) == PTR_TYPE_CONS) {
    if (car(car(curr)) == sym) {
      *res = cdr(car(curr));
      return 1;
    }
    curr = cdr(curr);
  }
  return 0;
}


int env_modify_binding(uint32_t env, uint32_t key, uint32_t val) {

  uint32_t curr = env;

  while (TYPE_OF(curr) == PTR_TYPE_CONS) {   
    if (car(car(curr)) == key) {
      set_cdr(car(curr), val); 
      return 1; 
    }
    curr = cdr(curr);
    
  }
  return 0;   
}


int env_build_params_args(uint32_t params,
			  uint32_t args,
			  uint32_t env0,
			  uint32_t *res_env) {
  uint32_t curr_param = params;
  uint32_t curr_arg = args;

  if (length(params) != length(args)) // programmer error
    return 0; 

  uint32_t env = env0;
  while (TYPE_OF(curr_param) == PTR_TYPE_CONS) {

    uint32_t entry = cons(car(curr_param), car(curr_arg));
    env = cons(entry,env);

    if (TYPE_OF(env) == VAL_TYPE_SYMBOL &&
	DEC_SYM(env) == symrepr_nil())
      return 0; 
    
    curr_param = cdr(curr_param);
    curr_arg   = cdr(curr_arg); 
  }
  *res_env = env;
  return 1;
}
