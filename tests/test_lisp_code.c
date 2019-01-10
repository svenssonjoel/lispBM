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

  mpc_ast_t* ast = NULL; 
  int res = 0; 

  if (argc < 2) {
    printf("Incorrect arguments\n"); 
    return 0;
  }
  
  FILE* fp = fopen(argv[1], "r");

  if (fp == NULL) {
    printf("Error opening file\n"); 
    return 0; 
  }

  fseek(fp, 0, SEEK_END); 
  size_t size = ftell(fp); 
  fseek(fp, 0, SEEK_SET);
  char *code_buffer = malloc(size * sizeof(char) + 1);
  size_t r = fread (code_buffer, 1, size, fp);

  if (r == 0) {
    printf("Error empty file?\n");
    return 0;
  }
  
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
  
  ast = parser_parse_string(code_buffer); 
  if (!ast) {
    printf("ERROR! parsing lisp:\n");
    return 0;
  }
  
  uint32_t t; 
  t = read_ast(ast);
  mpc_ast_delete(ast);

  printf("I: "); simple_print(t); printf("\n"); 
  
  t = eval_program(t);
  
  printf("O: "); simple_print(t); printf("\n");

  if ( DEC_SYM(t) == symrepr_eerror()) {
    res = 0;
  }
  
  uint32_t rest = t;
  while (length(rest) > 2) {
    rest = cdr(rest);
  }
  
  
  if (res && structural_equality(car(rest),car(cdr(rest)))) {
    printf("Test: OK!\n"); 
    res = 1;
  } else {
    printf("Test: Failed!\n"); 
    res = 0; 
  }  
  parser_del();
  symrepr_del();
  heap_del();
  
  return res;  
}
