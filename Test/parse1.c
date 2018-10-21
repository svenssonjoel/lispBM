
#include <stdlib.h>
#include <stdio.h>

#include "mpc.h"
#include "parse.h"

#include "symbol0.h" 
#include "read0.h"
 
int main(int argc, char **argv) {
  char *str = malloc(1024);;
  size_t len;

  mpc_ast_t* ast = NULL; 

  printf("OK SO FAR\n"); 
  init_parser();
  init_symtab(); 

  add_symbol("apa");
  add_symbol("bepa");
  add_symbol("cepa");
  add_symbol("depa");

  for (int i = 0; i < 2048; i ++) {
    char str[256];
    sprintf(str,"a%d",i);
    add_symbol(str); 
  }
  
  print_symtab();
  
  while (1) {
    
    getline(&str,&len,stdin);

    ast = parse_string(str); 
    if (!ast) {
      printf("ERROR!\n");
      break;
    }
    mpc_ast_print(ast);
    mpc_ast_delete(ast);
  }
  
  del_parser();
  del_symtab(); 
  return 0;
  
}
