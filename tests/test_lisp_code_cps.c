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

#include "heap.h"
#include "symrepr.h"
#include "builtin.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"

int main(int argc, char **argv) {

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
  long size = ftell(fp);
  if (size <= 0) {
    printf("Error file empty %s\n", argv[1]);
    return 0;
  }
  fseek(fp, 0, SEEK_SET);
  char *code_buffer = malloc((unsigned long)size * sizeof(char) + 1);
  size_t r = fread (code_buffer, 1, (unsigned int)size, fp);

  if (r == 0) {
    printf("Error empty file?\n");
    return 0;
  }

  res = symrepr_init();
  if (res)
    printf("Symrepr initialized.\n");
  else {
    printf("Error initializing symrepr!\n");
    return 0;
  }

  unsigned int heap_size = 8 * 1024 * 1024;
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

  VALUE t;
  t = tokpar_parse(code_buffer);

  printf("I: "); simple_print(t); printf("\n");

  t = eval_cps_program(t);

  printf("O: "); simple_print(t); printf("\n");

  if ( dec_sym(t) == symrepr_eerror()) {
    res = 0;
  }


  if (res && type_of(t) == VAL_TYPE_SYMBOL && dec_sym(t) == symrepr_true()){ // structural_equality(car(rest),car(cdr(rest)))) {
    printf("Test: OK!\n");
    res = 1;
  } else {
    printf("Test: Failed!\n");
    res = 0;
  }
  symrepr_del();
  heap_del();

  return res;
}
