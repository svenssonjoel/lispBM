
#include <stdlib.h>
#include <string.h>

#include "mpc.h"

#include "heap0.h"
#include "parse.h"
#include "symtab.h"

enum result_type { R_CONS, R_INTEGER, R_FLOAT, R_SYMBOL , R_ERROR };

typedef struct s_read_result { 

  enum result_type r_type; 
  union {
    cons_t   *r_cons_cell;  
    int32_t  r_integer;
    float    r_float;
    uint32_t r_symbol;
  };

    
} read_result_t;


read_result_t read_internal(mpc_ast_t* t) {

  printf("Entering read_internal\n"); 
  read_result_t res;
  
  if (strstr(t->tag, "name")) {
    uint32_t symbol_id;
    
    if (symtab_lookup(t->contents, &symbol_id)) {
      res.r_type = R_SYMBOL;
      res.r_symbol = symbol_id; 
    }
    else if (symtab_addname(t->contents,&symbol_id)) {
      res.r_type = R_SYMBOL;
      res.r_symbol = symbol_id; 
    } else {
      res.r_type = R_ERROR;
    }
    return res;
  }

  if (strstr(t->tag, "integer")) {
    res.r_type = R_INTEGER; 
    res.r_integer = (uint32_t)atoi(t->contents);
    return res; 
  }

  if (strstr(t->tag, "float")) {
    res.r_type = R_FLOAT;
    res.r_float = (float)atof(t->contents);
    return res;
  }

  if (strcmp(t->tag, ">") == 0) {
    int n = t->children_num;

    /* A program is a sequence of sexprs. 
       So represent it as a list .. ? */
    read_result_t tmp; 
    cons_t *root = NULL; 
    cons_t *curr = NULL; 
    cons_t *prev = NULL; 

    for (int i = 0; i < t->children_num; i ++) {

      tmp = read_internal(t->children[i]);

      switch(tmp.r_type) {
      case R_ERROR:
	res.r_type = R_ERROR; 
	return res;
	break;
	
      case R_SYMBOL: /* This would be a program with a "naked" symbol (not in an sexpr) */ 
	res.r_type = R_ERROR; 
	return res;
	break;
	
      case R_INTEGER: /* Naked integer in program */ 
	res.r_type = R_ERROR; 
	return res;
	break;
	
      case R_FLOAT: /* Naked float */ 
	res.r_type = R_ERROR; 
	return res;
	break;
	
      case R_CONS: /* First "real" program case */ 

	if (root == NULL) { /* create the head of list */
	  printf("allocating head cell for program\n"); 
	  root = heap_allocate_cell();
	  curr = root;
	  prev = NULL;
	}
	if (curr == NULL) {
	  curr = heap_allocate_cell();

	  prev->type = SET_CDR_TYPE(prev->type, POINTER);
	  prev->cdr.cons = curr; 
	}
	uint32_t type = 0;
	type = SET_CAR_TYPE(type, POINTER);
	type = SET_CDR_TYPE(type, NIL); 
	curr->car.cons = tmp.r_cons_cell;
	curr->cdr.i = 0; /*hack*/ 
	curr->type = type;
	
	prev = curr;
	curr = NULL;
	break;
	
      default:
	res.r_type = R_ERROR;
	return res;
	break;
      } 
    }
    res.r_type = R_CONS;
    res.r_cons_cell = root;
    return res; 
  }
  
  if (strstr(t->tag, "sexp")) {
    int n = t->children_num;

    /* A program is a sequence of sexprs. 
       So represent it as a list .. ? */
    read_result_t tmp; 
    cons_t *root = NULL; 
    cons_t *curr = NULL;
    cons_t *prev = NULL; 

    uint32_t type = 0;
    
    for (int i = 0; i < t->children_num; i ++) {

      if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
      if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
      if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
      
      tmp = read_internal(t->children[i]);


      if (root == NULL) { /* create the head of list */
	printf("allocating head node for sexp\n"); 
	root = heap_allocate_cell();
	curr = root;
	prev = NULL;
      }
      if (curr == NULL) {
	curr = heap_allocate_cell();
	
	prev->type = SET_CDR_TYPE(prev->type, POINTER);
	prev->cdr.cons = curr; 
      }
      
      switch(tmp.r_type) {
      case R_ERROR:
	res.r_type = R_ERROR; 
	return res;
	break;
	
      case R_SYMBOL:
	type = 0; 
	type = SET_CAR_TYPE(type, SYMBOL);
	type = SET_CDR_TYPE(type, NIL); 
	curr->car.s = tmp.r_symbol;
	curr->cdr.i = 0; /* hack */
	curr->type = type; 
	break;
	
      case INTEGER:
	type = 0;
	type = SET_CAR_TYPE(type, INTEGER);
	type = SET_CDR_TYPE(type, NIL); 
	curr->car.i = tmp.r_integer;
	curr->cdr.i = 0; /* hack */
	curr->type = type; 
	break;
	
      case R_FLOAT:
	type = 0;
	type = SET_CAR_TYPE(type, FLOAT);
	type = SET_CDR_TYPE(type, NIL); 
	curr->car.f = tmp.r_float;
	curr->cdr.i = 0; /* hack */
	curr->type = type; 
	break;
	
      case R_CONS: /* First "real" program case */ 

	type = 0;
	type = SET_CAR_TYPE(type, POINTER);
	type = SET_CDR_TYPE(type, NIL); 
	curr->car.cons = tmp.r_cons_cell;
	curr->cdr.i = 0; /*hack*/ 
	curr->type = type;
	break;
	
      default:
	res.r_type = R_ERROR;
	return res;
	break;
      }
      prev = curr;
      curr = NULL;
    }
    res.r_type = R_CONS;
    res.r_cons_cell = root;
    return res;
  } 
}

cons_t* read_ast(mpc_ast_t *t) {

  read_result_t res = read_internal(t);

  if (res.r_type == R_CONS)
    return res.r_cons_cell;
  else
    printf ("Error reading ast\n"); 
  return NULL;
}




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

/*
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
    cell->cdr.i = 0; 
  }
  else if (strcmp(t->tag, ">") == 0) {
    int n = t->children_num;

    cons_t *root = NULL; 

    for (int i = 0; i < t->children_num; i ++) {
      cons_t *curr_tramp = heap_allocate_cell();
      cons_t *curr = read_ast(t->children[i]);
      type = SET_CAR_TYPE(type,POINTER);
      type = SET_CDR_TYPE(type,NIL);
      curr_tramp->type = type;
      curr_tramp->car.cons = curr;
      curr_tramp->cdr.i = 0; 
      root = concat(root,curr_tramp); 
    }
    return root; 
  }
  else if (strstr(t->tag, "sexp")) {
    int n = t->children_num;

    cons_t *sexp = NULL; 
    
    for (int i = 0; i < n; i ++) {
      if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
      if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
      if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
      
      sexp = concat(sexp, read_ast(t->children[i])); 
    }
    return sexp;
  }
  else {
    printf("CATCH ALL BRANCH!\n");
    printf("%s\n", t->tag); 
  }
  
  return cell;  
}
 */
