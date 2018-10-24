
#include <stdlib.h>
#include <string.h>

#include "mpc.h"

#include "heap0.h"
#include "parse.h"
#include "symtab.h"

int read_integer(mpc_ast_t* t) {
  return atoi(t->contents);
}

cons_t* concat(cons_t *h, cons_t *t) {
  
  if (h == NULL) return t;
  if (t == NULL) return h;

  cons_t *curr = h; 
  
  while (GET_CDR_TYPE(curr->type) == POINTER) {
    curr = curr->cdr.cons;
  }
  
  if (GET_CDR_TYPE(curr->type) != NIL) {
    printf("ERROR: DEAL WITH THIS BETTER LATER!\n");
    return NULL;
  }

  curr->type = SET_CDR_TYPE(curr->type,POINTER); 
  curr->cdr.cons = t;
  
  return h; 
}


cons_t* read_ast(mpc_ast_t *t) {

  cons_t* cell = NULL; 
  uint32_t type = 0; 

  if (strstr(t->tag, "name")) {
    cell = heap_allocate_cell();
    uint32_t symbol_id; 
    type = SET_CDR_TYPE(type, NIL);
    type = SET_CAR_TYPE(type, SYMBOL);
    cell->type = type;

    if (symtab_lookup(t->contents, &symbol_id)) {
      cell->car.s = symbol_id;
    }
    else if (symtab_addname(t->contents,&symbol_id)) {
      cell->car.s = symbol_id; 
    } else {
      return NULL;
    }  
  }
  else if (strstr(t->tag, "integer")) {
    cell = heap_allocate_cell();
    type = SET_CDR_TYPE(type, NIL);
    type = SET_CAR_TYPE(type, INTEGER);
    cell->type = type;
    cell->car.i = read_integer(t); 
    cell->cdr.i = 0; /* hack */ 
  }
  else if (strcmp(t->tag, ">") == 0) {
    int n = t->children_num;

    cons_t *root = NULL; 

    for (int i = 0; i < t->children_num; i ++) {
      cons_t *curr_tramp = heap_allocate_cell();
      cons_t *curr = read_ast(t->children[i]);
      /* TODO: CHECKING OF EXCEPTIONAL CONDITIONS! */ 
      type = SET_CAR_TYPE(type,POINTER);
      type = SET_CDR_TYPE(type,NIL);
      curr_tramp->type = type;
      curr_tramp->car.cons = curr;
      curr_tramp->cdr.i = 0; /* hack */ 
      root = concat(root,curr_tramp); 
    }
    return root; 
  }
  else if (strstr(t->tag, "sexp")) {
    int n = t->children_num;

    cons_t *sexp = NULL; 
    
    for (int i = 0; i < n; i ++) {
      /* Some conditions here */ 
      if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
      if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
      if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
      sexp = concat(sexp, read_ast(t->children[i])); 
    }
    /* TODO: more checking has to be done */ 
    return sexp;
  }
  else {
    printf("CATCH ALL BRANCH!\n");
    printf("%s\n", t->tag); 
  }

  /* test */
  // if (cell) {
  //  printf("CAR_TYPE: %d\n", GET_CAR_TYPE(cell->type));
  //  printf("CDR_TYPE: %d\n", GET_CDR_TYPE(cell->type)); 
  //}
  
  return cell;  
}
