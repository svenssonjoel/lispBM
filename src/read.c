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

#include <stdlib.h>
#include <string.h>

#include "mpc.h"

#include "read.h"
#include "heap.h"
#include "parse.h"
#include "symrepr.h"

uint32_t xtou(char *str) {
  return (uint32_t)strtoul(str,NULL,0);
}


uint32_t read_ast(mpc_ast_t *t){

  uint32_t rerror = symrepr_rerror();
  uint32_t nil    = symrepr_nil(); 
  uint32_t quote  = symrepr_quote(); 
  
  // Base cases
  if (strstr(t->tag, "name")) {
    uint32_t symbol_id;
    
    if (symrepr_lookup(t->contents, &symbol_id)) {
      return ENC_SYM(symbol_id);  
    }
    else if (symrepr_addsym(t->contents,&symbol_id)) {
      return ENC_SYM(symbol_id);  
    } else {
      return ENC_SYM(rerror); 
    }
  }

  // READ Integers
  if (strstr(t->tag, "integer")) {
    if (strlen(t->contents) > 2 &&
	t->contents[0] == '0' &&
	t->contents[1] == 'x' ) {
      uint32_t v = (uint32_t)xtou(t->contents);
      if (strlen(t->contents) == 10) { // Boxed 32 bit uint
	uint32_t ptr_cons = cons(v,ENC_SYM(nil));
	uint32_t ptr_uint_box = SET_PTR_TYPE(ptr_cons, PTR_TYPE_U32);
	return ptr_uint_box;
      } else {
	return ENC_U28(v);
      }
    }
    if (t->contents[strlen(t->contents)-1] == 'u') {
      uint32_t v = (uint32_t)atoi(t->contents);
      return ENC_U28(v);
    }
    int32_t v = (int32_t)atoi(t->contents);
    return ENC_I28(v); 
  }

  if (strstr(t->tag, "float")) {
    float v = (float)atof(t->contents);
    uint32_t uv = *(uint32_t*)(&v);
    uint32_t ptr_cons = cons(uv,ENC_SYM(nil)); // Boxed value.
    uint32_t ptr_float_box = SET_PTR_TYPE(ptr_cons, PTR_TYPE_F32);
    return ptr_float_box;
  }

  // Program case
  if (strcmp(t->tag, ">") == 0) {
    int n = t->children_num;
    uint32_t res = ENC_SYM(nil);
    
    for (int i = n - 1; i >= 0; i --) {
      if (strcmp(t->children[i]->tag, "regex") == 0) { continue; }
      if (strstr(t->children[i]->tag, "comment")) { continue; }
       
      uint32_t r = read_ast(t->children[i]);
      res = cons(r, res); 
    }
    return res; 
  }

  // Read SExpr 
  if (strstr(t->tag, "sexp")) {
    int n = t->children_num;
    uint32_t res = ENC_SYM(nil);
    
    if (t->children_num == 5 &&
	strcmp(t->children[0]->contents, "(") == 0 &&
	strcmp(t->children[2]->contents, ".") == 0 &&
	strcmp(t->children[4]->contents, ")") == 0 ) {
      uint32_t a = read_ast(t->children[1]);
      uint32_t b = read_ast(t->children[3]);
      return cons(a,b); 
      
    }
      
    for (int i = n-1; i >= 0; i --) {

      if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
      if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
      if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
      
      
      uint32_t r = read_ast(t->children[i]);
      res = cons(r, res); 
    }
    return res;
  }
  
  // Read QExpr 
  if (strstr(t->tag, "qexp")) {
    int n = t->children_num;
    uint32_t res = ENC_SYM(nil);

    if (n != 2) {
      return res;
    }

    uint32_t r = read_ast(t->children[1]);
    res = cons(ENC_SYM(quote), cons(r, res));
    
    return res;
  }  
  return ENC_SYM(nil);
}
