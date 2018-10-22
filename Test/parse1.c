
#include <stdlib.h>
#include <stdio.h>

#include "mpc.h"
#include "parse.h"

#include "heap0.h" 
#include "symbol0.h" 
#include "read0.h"


int main(int argc, char **argv) {
  char *str = malloc(1024);;
  size_t len;

  mpc_ast_t* ast = NULL; 
  int res = 0; 

  res = parser_init();
  if (res) 
    printf("Parser initialized.\n");
  else { 
    printf("Error initializing parser!\n");
    return 0;
  }
  
  res = heap_init(10000);
  if (res)
    printf("Heap initialized.\n");
  else {
    printf("Error initializing heap!\n");
    return 0;
  }

  res = symtab_init();
  if (res) 
    printf("Symtab initialized.\n");
  else {
    printf("Error initializing symtab!\n");
    return 0;
  }
  uint32_t id;
  
  symtab_addsym("apa",&id);
  printf("symid: %d\n", id); 
  symtab_addsym("bepa",&id);
  printf("symid: %d\n", id); 
  symtab_addsym("cepa",&id);
  printf("symid: %d\n", id); 
  symtab_addsym("depa",&id);
  printf("symid: %d\n", id);
    
  symtab_print();

  cons_t *c1 = heap_allocate_cell();
  cons_t *c2 = heap_allocate_cell(); 
  
  printf("HEAP has %ld free cons cells\n", heap_num_free()); 
  
  
  while (1) {
    
    getline(&str,&len,stdin);

    ast = parser_parse_string(str); 
    if (!ast) {
      printf("ERROR!\n");
      break;
    }
    mpc_ast_print(ast);
    mpc_ast_delete(ast);
  }
  
  parser_del();
  symtab_del(); 
  return 0;
  
}
