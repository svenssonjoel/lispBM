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

  printf("Lisp REPL started!\n"); 
  
  SYMBOL_NIL = symrepr_nil(); 
  
  // Setup the initial global env
  eval_set_env(built_in_gen_env());
  
  while (1) {
    printf("# "); 
    size_t n =  getline(&str,&len,stdin);

    if (strncmp(str, "info", 4) == 0) {
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
    } else {
    
      ast = parser_parse_string(str); 
      if (!ast) {
	printf("ERROR!\n");
	break;
      }
    
      uint32_t t;
      t = read_ast(ast);
    
      t = eval_program(t);
      
      printf("> "); esimple_print(t); printf("\n");
        
      mpc_ast_delete(ast);
    }
  }
  
  parser_del();
  symrepr_del();
  heap_del();
  
  return 0;  
}
