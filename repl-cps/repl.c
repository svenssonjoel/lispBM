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

#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <stdio.h>

#include "heap.h" 
#include "symrepr.h"
#include "builtin.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"

int main(int argc, char **argv) {
  char *str = malloc(1024);;
  size_t len;
  int res = 0; 

  heap_state_t heap_state;

  res = symrepr_init();
  if (res) 
    printf("Symrepr initialized.\n");
  else {
    printf("Error initializing symrepr!\n");
    return 0;
  }

  unsigned int heap_size = 512*512;
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

  res = eval_cps_init();
  if (res)
    printf("Evaluator initialized.\n");
  else {
    printf("Error initializing evaluator.\n");
  }

  printf("Lisp REPL started!\n");
  printf("Type :quit to exit.\n");
  printf("     :info for statistics.\n");
  
  while (1) {
    printf("# "); 
    ssize_t n = getline(&str,&len,stdin);

    if (n >= 5 && strncmp(str, ":info", 5) == 0) {
      printf("############################################################\n");
      printf("Used cons cells: %d\n", heap_size - heap_num_free());
      printf("ENV: "); simple_print(eval_cps_get_env()); printf("\n"); 
      //symrepr_print();
      //heap_perform_gc(eval_cps_get_env());
      heap_get_state(&heap_state);
      printf("Allocated arrays: %u\n", heap_state.num_alloc_arrays);
      printf("GC counter: %d\n", heap_state.gc_num);
      printf("Recovered: %d\n", heap_state.gc_recovered);
      printf("Recovered arrays: %u\n", heap_state.gc_recovered_arrays);
      printf("Marked: %d\n", heap_state.gc_marked);
      printf("Free cons cells: %d\n", heap_num_free());
      printf("############################################################\n");
    } else  if (n >= 5 && strncmp(str, ":quit", 5) == 0) {
      break;
    } else {
      
      VALUE t;
      t = tokpar_parse(str);
      
      t = eval_cps_program(t);
      
      if (dec_sym(t) == symrepr_eerror()) {
	printf("Eval error\n"); 
      } else {
	printf("> "); simple_print(t); printf("\n");
      }
    }
  }
  
  symrepr_del();
  heap_del();
  
  return 0;  
}
