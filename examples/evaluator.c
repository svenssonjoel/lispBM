/*
    Copyright 2018, 2021 Joel Svensson	svenssonjoel@yahoo.se

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
#include <unistd.h>
#include <ctype.h>
#include <getopt.h>

#include "heap.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "compression.h"
#include "memory.h"

#define EVAL_CPS_STACK_SIZE 256

int main(int argc, char **argv) {

  int res = 0;

  unsigned int heap_size = 8 * 1024 * 1024;  // 8 Megabytes is standard
  bool growing_continuation_stack = false;
  bool compress_decompress = false;

  int c;
  opterr = 1;

  while (( c = getopt(argc, argv, "gch:")) != -1) {
    switch (c) {
    case 'h':
      heap_size = (unsigned int)atoi((char *)optarg);
      break;
    case 'g':
      growing_continuation_stack = true;
      break;
    case 'c':
      compress_decompress = true;
      break;
    case '?':
      break;
    default:
      break;
    }
  }
  printf("------------------------------------------------------------\n");
  printf("Heap size: %u\n", heap_size);
  printf("Growing stack: %s\n", growing_continuation_stack ? "yes" : "no");
  printf("Compression: %s\n", compress_decompress ? "yes" : "no");
  printf("------------------------------------------------------------\n");
	 
  if (argc - optind < 1) {
    printf("Incorrect arguments\n");
    return 0;
  }

  printf("Opening file: %s\n", argv[optind]);

  FILE* fp = fopen(argv[optind], "r");

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

  unsigned char *memory = malloc(MEMORY_SIZE_16K);
  unsigned char *bitmap = malloc(MEMORY_BITMAP_SIZE_16K);
  if (memory == NULL || bitmap == NULL) return 0;
  
  res = lbm_memory_init(memory, MEMORY_SIZE_16K,
		    bitmap, MEMORY_BITMAP_SIZE_16K);
  if (res)
    printf("Memory initialized. Memory size: %u Words. Free: %u Words.\n", lbm_memory_num_words(), lbm_memory_num_free());
  else {
    printf("Error initializing memory!\n");
    return 0;
  }
  
  res = lbm_symrepr_init();
  if (res)
    printf("Symrepr initialized.\n");
  else {
    printf("Error initializing symrepr!\n");
    return 0;
  }
  
  res = lbm_heap_init(heap_size);
  if (res)
    printf("Heap initialized. Heap size: %f MiB. Free cons cells: %d\n", lbm_heap_size_bytes() / 1024.0 / 1024.0, lbm_heap_num_free());
  else {
    printf("Error initializing heap!\n");
    return 0;
  }

  res = eval_cps_init_nc(EVAL_CPS_STACK_SIZE, false);
  if (res)
    printf("Evaluator initialized.\n");
  else {
    printf("Error initializing evaluator.\n");
  }

  lbm_value prelude = prelude_load();
  eval_cps_program_nc(prelude);

  lbm_value t;
  
  if (compress_decompress) { 
    uint32_t compressed_size = 0;
    char *compressed_code = lbm_compress(code_buffer, &compressed_size);
    if (!compressed_code) {
      printf("Error compressing code\n");
      return 0;
    }
    char decompress_code[8192];
    lbm_decompress(decompress_code, 8192, compressed_code);
    printf("\n\nDECOMPRESS TEST: %s\n\n", decompress_code);
    
    t = tokpar_parse_compressed(compressed_code);
    free(compressed_code);
  } else { 
    t = tokpar_parse(code_buffer);
  } 
  //printf("I: "); simple_print(t); printf("\n");

  t = eval_cps_program_nc(t);

  char output[1024];
  char error[1024];
  
  int v =  lbm_print_value(output,1024,error,1024,t);

  if (v >= 0) {
    printf("> %s\n", output);
  } else {
    printf("> %s\n", error);
  }
  
  symrepr_del();
  heap_del();

  return v;
}
