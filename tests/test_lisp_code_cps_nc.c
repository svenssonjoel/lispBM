/*
    Copyright 2018,2020,2021 Joel Svensson	svenssonjoel@yahoo.se

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
#include <unistd.h>
#include <ctype.h>
#include <getopt.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>

#include "heap.h"
#include "symrepr.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "compression.h"
#include "lispbm_memory.h"
#include "extensions.h"

#define EVAL_CPS_STACK_SIZE 256

/* Tokenizer for strings */
static tokenizer_string_state_t string_tok_state;
static tokenizer_char_stream string_tok;

/* Tokenizer for compressed data */
static tokenizer_compressed_state_t comp_tok_state;
static tokenizer_char_stream comp_tok;

VALUE ext_even(VALUE *args, UINT argn) {

  if (argn < 1) return enc_sym(SYM_NIL);

  VALUE v = args[0];

  if (val_type(v) == VAL_TYPE_I ||
      val_type(v) == VAL_TYPE_U) {
    if (dec_i(v) % 2 == 0)
      return enc_sym(SYM_TRUE);
  }

  return enc_sym(SYM_NIL);
}

VALUE ext_odd(VALUE *args, UINT argn) {

  if (argn < 1) return enc_sym(SYM_NIL);

  VALUE v = args[0];

  if (val_type(v) == VAL_TYPE_I ||
      val_type(v) == VAL_TYPE_U) {
    if (dec_i(v) % 2 == 1)
      return enc_sym(SYM_TRUE);
  }

  return enc_sym(SYM_NIL);
}


int main(int argc, char **argv) {

  int res = 0;

  unsigned int heap_size = 8 * 1024 * 1024;  // 8 Megabytes is standard
  bool compress_decompress = false;

  cons_t *heap_storage = NULL;

  int c;
  opterr = 1;

  while (( c = getopt(argc, argv, "gceh:")) != -1) {
    switch (c) {
    case 'h':
      heap_size = (unsigned int)atoi((char *)optarg);
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

  uint32_t *memory = malloc(4 * MEMORY_SIZE_16K);
  if (memory == NULL) return 0;
  uint32_t *bitmap = malloc(4 * MEMORY_BITMAP_SIZE_16K);
  if (memory == NULL) return 0;

  res = memory_init(memory, MEMORY_SIZE_16K,
                    bitmap, MEMORY_BITMAP_SIZE_16K);
  if (res)
    printf("Memory initialized.\n");
  else {
    printf("Error initializing memory!\n");
    return 0;
  }

  res = symrepr_init();
  if (res)
    printf("Symrepr initialized.\n");
  else {
    printf("Error initializing symrepr!\n");
    return 0;
  }

  heap_storage = (cons_t*)malloc(sizeof(cons_t) * heap_size);
  if (heap_storage == NULL) {
    return 0;
  }

  res = heap_init(heap_storage, heap_size);
  if (res)
    printf("Heap initialized. Heap size: %f MiB. Free cons cells: %d\n", heap_size_bytes() / 1024.0 / 1024.0, heap_num_free());
  else {
    printf("Error initializing heap!\n");
    return 0;
  }

  res = extensions_add("ext-even", ext_even);
  if (res)
    printf("Extension added.\n");
  else {
    printf("Error adding extension.\n");
    return 0;
  }

  res = extensions_add("ext-odd", ext_odd);
  if (res)
    printf("Extension added.\n");
  else {
    printf("Error adding extension.\n");
    return 0;
  }

  res = eval_cps_init_nc(EVAL_CPS_STACK_SIZE);
  if (res)
    printf("Evaluator initialized.\n");
  else {
    printf("Error initializing evaluator.\n");
  }

  prelude_load(&string_tok_state, &string_tok);
  VALUE prelude = tokpar_parse_program(&string_tok);

  eval_cps_program_nc(prelude);


  VALUE t;

  if (compress_decompress) {
    uint32_t compressed_size = 0;
    char *compressed_code = compression_compress(code_buffer, &compressed_size);
    if (!compressed_code) {
      printf("Error compressing code\n");
      return 0;
    }
    char decompress_code[8192];
    compression_decompress(decompress_code, 8192, compressed_code);
    printf("\n\nDECOMPRESS TEST: %s\n\n", decompress_code);

    compression_create_char_stream_from_compressed(&comp_tok_state,
                                                   &comp_tok,
                                                   compressed_code);
    t = tokpar_parse(&comp_tok);
    free(compressed_code);
  } else {
    tokpar_create_char_stream_from_string(&string_tok_state,
                                          &string_tok,
                                          code_buffer);
    t = tokpar_parse(&string_tok);
  }

  char output[128];

  res = print_value(output, 128, t);

  if ( res >= 0) {
    printf("I: %s\n", output);
  } else {
    printf("%s\n", output);
    return 0;
  }

  if (type_of(t) == VAL_TYPE_SYMBOL &&
      dec_sym(t) == SYM_MERROR) {
    printf("out of memory while parsing\n");
    return 0;
  }

  t = eval_cps_program_nc(t);

  res = print_value(output, 128, t);

  if ( res >= 0) {
    printf("O: %s\n", output);
  } else {
    printf("%s\n", output);
    return 0;
  }

  if ( dec_sym(t) == SYM_EERROR) {
    res = 0;
  }


  if (res && type_of(t) == VAL_TYPE_SYMBOL && dec_sym(t) == SYM_TRUE){ // structural_equality(car(rest),car(cdr(rest)))) {
    printf("Test: OK!\n");
    res = 1;
  } else {
    printf("Test: Failed!\n");
    res = 0;
  }

  free(heap_storage);

  return res;
}
