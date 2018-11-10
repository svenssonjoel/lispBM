
#include <stdlib.h>
#include <stdio.h>

#include "mpc.h"
#include "parse.h"

#include "heap0.h" 
//#include "read0.h"
//#include "rbtree.h"
#include "symtab.h"
//#include "built_in.h"
//#include "eval0.h"
//#include "print.h"

//void test_stuff(void);

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

  res = symtab_init();
  if (res) 
    printf("Symtab initialized.\n");
  else {
    printf("Error initializing symtab!\n");
    return 0;
  }

  uint32_t nil_sym; 
  symtab_addname("nil", &nil_sym);
  
  res = heap_init(8 * 1024 * 1024, nil_sym);
  if (res)
    printf("Heap initialized. Heap size: %f MiB. Free cons cells: %d\n", heap_size_bytes() / 1024.0 / 1024.0, heap_num_free());
  else {
    printf("Error initializing heap!\n");
    return 0;
  }



  symtab_print();
  
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



  
  
  //uint32_t c1 = heap_allocate_cell();
  //uint32_t c2 = heap_allocate_cell(); 

  //  printf("cell1:  %x\n", c1);
  // printf("cell2:  %x\n", c2); 
  //printf("HEAP has %d free cons cells\n", heap_num_free()); 

  
  //uint32_t num_free = heap_num_free();
   
  //for (uint32_t i = 0; i < 9000000; i ++) {
  //uint32_t c = heap_allocate_cell();
    //printf("%u : c%u : %x\n",heap_num_free(), i, c);
  //}
    
  //printf("HEAP has %d free cons cells\n", heap_num_free()); 
  
  /*
 


  res = built_in_init();
  if (res)
    printf("Built-in functions initialized.\n");
  else {
    printf("Error initializing built-in functions!\n");
    return 0;
  }
  */
  //printf("\n\nRUNNING TESTS\n\n");
  //test_stuff();
  

  /*
  while (1) {
    
    getline(&str,&len,stdin);

    ast = parser_parse_string(str); 
    if (!ast) {
      printf("ERROR!\n");
      break;
    }

    //mpc_ast_print(ast);

    cons_t *t = NULL; 
    if (!(t = read_ast(ast))) {
      printf("read_ast failed!\n");
      return 0; 
    }

    t = eval(t);
    
    simple_print(t); 
     
    printf("\n"); 
   
    mpc_ast_delete(ast);
    printf("############################################################\n"); 
    printf("HEAP has %ld free cons cells\n", heap_num_free());
    symtab_print();
    printf("############################################################\n"); 
  }
  */
  parser_del();
  //  symtab_del(); 
  return 0;
  
}

/*
void test_stuff(void) {
  uint32_t id;
   
  printf("Adding symbols to symtab:\n\n"); 
  symtab_addname("apa",&id);
  printf("symid: %d\n", id); 
  symtab_addname("bepa",&id);
  printf("symid: %d\n", id); 
  symtab_addname("cepa",&id);
  printf("symid: %d\n", id); 
  symtab_addname("depa",&id);
  printf("symid: %d\n", id);

  symtab_addname("apa",&id);

  printf("\n\nPrinting Symtab\n\n"); 
  symtab_print();


  printf("\n\nLooking up IDS\n\n"); 
  
  uint32_t apa_id, bepa_id, cepa_id, depa_id; 
  
  if (symtab_lookup("apa",&apa_id)) 
    printf("Success: apa = %d\n", apa_id); 
  else
    printf("Failed: apa\n");

  if (symtab_lookup("bepa",&bepa_id)) 
    printf("Success: bepa = %d\n", bepa_id); 
  else
    printf("Failed: bepa\n");

  if (symtab_lookup("cepa",&cepa_id)) 
    printf("Success: cepa = %d\n", cepa_id); 
  else
    printf("Failed: cepa\n");

  if (symtab_lookup("depa",&depa_id)) 
    printf("Success: depa = %d\n", depa_id); 
  else
    printf("Failed: depa\n");

  char*t; 

  printf("\n\nLooking up names\n\n"); 
  
  if ((t = symtab_lookup_name(apa_id)) != NULL)
    printf("lookup apa == %s\n",t);
  else
    printf("FAILED\n");
  if ((t = symtab_lookup_name(bepa_id)) != NULL)
    printf("lookup bepa == %s\n",t);
  else
    printf("FAILED\n");
  if ((t = symtab_lookup_name(cepa_id)) != NULL)
    printf("lookup cepa == %s\n",t);
  else
    printf("FAILED\n");
  if ((t = symtab_lookup_name(depa_id)) != NULL)
    printf("lookup depa == %s\n",t);
  else
    printf("FAILED\n");

  printf("\n\nLooking up built-in functions\n\n"); 
  
  if (symtab_lookup("+",&id)) 
    printf("Lookup +: %d\n", id);
  else
    printf("lookup +: FAILED!\n"); 

  if (symtab_lookup("-",&id)) 
    printf("Lookup -: %d\n", id);
  else
    printf("lookup -: FAILED!\n"); 

  if (symtab_lookup("*",&id)) 
    printf("Lookup *: %d\n", id);
  else
    printf("lookup *: FAILED!\n"); 


  if (symtab_lookup("/",&id)) 
    printf("Lookup /: %d\n", id);
  else
    printf("lookup /: FAILED!\n"); 

  if (symtab_lookup("DEFINE",&id)) 
    printf("Lookup DEFINE: %d\n", id);
  else
    printf("lookup DEFINE: FAILED!\n"); 


  printf("\n\nHeap allocation \n\n"); 
  
  cons_t *c1 = heap_allocate_cell();
  cons_t *c2 = heap_allocate_cell(); 
  
  printf("HEAP has %ld free cons cells\n", heap_num_free()); 


  printf("\n\nRed black tree\n\n"); 
  
  cons_t *tree = rbtree_create();
  
  printf("HEAP has %ld free cons cells\n", heap_num_free());

  
  if (rbtree_is_empty(tree)) {
    printf("Tree is empty! Empty tree represented by NIL type on car field\n");
  } else {
    printf("Error: The tree is not considered empty\n");
  }
}
*/
