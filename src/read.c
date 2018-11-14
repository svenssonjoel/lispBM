
#include <stdlib.h>
#include <string.h>

#include "mpc.h"

#include "read.h"
#include "heap.h"
#include "parse.h"
#include "symrepr.h"

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

  // need to tell difference between signed and unsigned. Maybe use a suffix?
  // For now only read signed integers. 
  if (strstr(t->tag, "integer")) {
    int32_t v = (int32_t)atoi(t->contents);
    return ENC_I28(v); 
  }

  // Program case
  if (strcmp(t->tag, ">") == 0) {
    int n = t->children_num;
    uint32_t res = ENC_SYM(nil);
    
    for (int i = n - 1; i >= 0; i --) {
      if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
       
      uint32_t r = read_ast(t->children[i]);
      res = cons(r, res); 
    }
    return res; 
  }

  // Read SExpr 
  if (strstr(t->tag, "sexp")) {
    int n = t->children_num;
    uint32_t res = ENC_SYM(nil);
    
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
