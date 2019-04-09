#include <stdlib.h>
#include <stdio.h>

#include "heap.h"
#include "symrepr.h"

                  
		  
#define TEST_SIZE ((65534 - 49999) * 4096)

int main(int argc, char **argv) {

  int res = 1; 
  char *name;
  
  res = symrepr_init();
  if (!res) {
    printf("Error initializing symrepr\n");
    return 0;
  }
  printf("Initialized symrepr: OK\n");

  uint32_t last_id = 0;
  uint32_t symbol_id; 
  uint32_t gs;
  printf("Gensyming %d symbols: ", TEST_SIZE);
  for (int i = 0; i < TEST_SIZE; i ++) { 
    res &= gensym(&gs);
    if (!res) {
      printf("Error running gensym\n");
      return 0;
    }
    //printf("generated sym: 0x%x\n", ENC_SYM(gs));
    name = symrepr_lookup_name(gs);
    //printf("SYMBOL: %s\n",name);
    symbol_id = (uint32_t)atoi(&name[7]);
    //printf("SYMBOL ID: %d",symbol_id);

    res &= (symbol_id == gs);
    if (!res) printf("Symbol name does not match gensym (%s) running number %u != %u \n", name, symbol_id, gs);
    res &= (symbol_id > last_id);
    if (!res) printf("Symbol id is not greater than previous symbol id %d %d \n", symbol_id, last_id); 
    last_id = symbol_id;
    if (res && i % 2000000 == 0){
      printf(".");
      fflush(stdout); 
    }
  }

  printf("OK!\n");
  
  int should_fail = gensym(&gs);
  if (should_fail == 0) {
    printf("gensyms exhausted: OK!\n"); 
  } else {
    printf("should_fail test did not fail!\n"); 
  }

  printf("Deleting symbol representation table\n"); 
  symrepr_del();
  
  return res; 

}

