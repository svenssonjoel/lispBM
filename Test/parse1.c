
#include <stdlib.h>
#include <stdio.h>

#include "mpc.h"
#include "parse.h"

#include "heap.h" 
#include "read.h"
#include "symrepr.h"
#include "builtin.h"
#include "eval.h"
#include "print.h"

int main(int argc, char **argv) {
  char *str = malloc(1024);;
  size_t len;

  mpc_ast_t* ast = NULL; 
  int res = 0; 

  heap_state_t heap_state;

  uint32_t SYMBOL_NIL;

  
  res = parser_init();
  if (res) 
    printf("Parser initialized.\n");
  else { 
    printf("Error initializing parser!\n");
    return 0;
  }

  res = symrepr_init();
  if (res) 
    printf("Symrepr initialized.\n");
  else {
    printf("Error initializing symrepr!\n");
    return 0;
  }
  int heap_size = 8 * 1024 * 1024;
  res = heap_init(heap_size);
  //res = heap_init(8 * 1024);
  if (res)
    printf("Heap initialized. Heap size: %f MiB. Free cons cells: %d\n", heap_size_bytes() / 1024.0 / 1024.0, heap_num_free());
  else {
    printf("Error initializing heap!\n");
    return 0;
  }

  res = builtin_init();
  if (res)
    printf("Built in functions initialized.\n");
  else {
    printf("Error initializing built in functions.\n");
    return 0;
  }

  res = eval_init();
  if (res)
    printf("Evaluator initialized.\n");
  else {
    printf("Error initializing evaluator.\n");
  }
  
  symrepr_print();
  SYMBOL_NIL = symrepr_nil(); 
  
  int n = 0;
  printf("DEC/ENC %d: %s \n", n++, (DEC_I28(ENC_I28(0)) == 0) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_I28(ENC_I28(-1)) == -1) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_I28(ENC_I28(1)) == 1) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_I28(ENC_I28(134217727)) == 134217727) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_I28(ENC_I28(-134217728)) == -134217728) ? "ok" : "NOK!");

  printf("DEC/ENC %d: %s \n", n++, (DEC_U28(ENC_U28(0)) == 0) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_U28(ENC_U28(1)) == 1) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_U28(ENC_U28(268435455)) == 268435455) ? "ok" : "NOK!");

  printf("DEC/ENC %d: %s \n", n++, (DEC_CHAR(ENC_CHAR(0)) == 0) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_CHAR(ENC_CHAR(-1)) == -1) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_CHAR(ENC_CHAR(1)) == 1) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_CHAR(ENC_CHAR(127)) == 127) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_CHAR(ENC_CHAR(-128)) == -128) ? "ok" : "NOK!");

  printf("DEC/ENC %d: %s \n", n++, (DEC_SYM(ENC_SYM(0)) == 0) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_SYM(ENC_SYM(1)) == 1) ? "ok" : "NOK!");
  printf("DEC/ENC %d: %s \n", n++, (DEC_SYM(ENC_SYM(268435455)) == 268435455) ? "ok" : "NOK!");


  printf("DEC/ENC %d: %s \n", n++, (DEC_SYM(ENC_SYM(symrepr_quote())) == symrepr_quote()) ? "ok" : "NOK!");
  
  simple_print(ENC_SYM(SYMBOL_NIL));
  printf("\n"); 


  // Setup the initial global env
  eval_set_env(built_in_gen_env());
  
  while (1) {
    
    size_t n =  getline(&str,&len,stdin);
    
    ast = parser_parse_string(str); 
    if (!ast) {
      printf("ERROR!\n");
      break;
    }
    
    //mpc_ast_print(ast);

    uint32_t t;
    t = read_ast(ast);

    simple_print(t); printf("\n");
    
    t = eval_program(t); 
    
    printf("> "); simple_print(t); 
    
    printf("\n"); 
    
    mpc_ast_delete(ast);
    printf("############################################################\n");
    printf("Used cons cells: %d \n", heap_size - heap_num_free());
    printf("ENV: "); simple_print(eval_get_env()); printf("\n"); 
    //symrepr_print();
    heap_perform_gc(eval_get_env());
    heap_get_state(&heap_state);
    printf("GC counter: %d\n", heap_state.gc_num);
    printf("Recovered: %d\n", heap_state.gc_recovered);
    printf("Marked: %d\n", heap_state.gc_marked);
    printf("Free cons cells: %d\n", heap_num_free());
    printf("############################################################\n");
  }
  
  parser_del();
  //  symtab_del(); 
  return 0;  
}
