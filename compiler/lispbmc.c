/*
    Copyright 2018, 2020, 2021 Joel Svensson	svenssonjoel@yahoo.se

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
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>
#include <getopt.h>
#include <termios.h>
#include <ctype.h>

#include "heap.h"
#include "symrepr.h"
#include "extensions.h"
#include "ec_eval.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "typedefs.h"
#include "memory.h"
#include "env.h"

#define EVAL_CPS_STACK_SIZE 256

#define HEAP_SIZE 8192

lbm_cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));

FILE *in_file = NULL;
FILE *out_file = NULL;

/*
 Extensions
 */
lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  if (argn < 1) return lbm_enc_sym(SYM_NIL);

  char output[1024];
  char error[1024];

  for (int i = 0; i < argn; i ++) {
    lbm_value t = args[i];

    if (lbm_is_ptr(t) && ptr_type(t) == LBM_PTR_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)lbm_car(t);
      switch (array->elt_type){
      case LBM_VAL_TYPE_CHAR: {
	char *data = (char *)array + 8;
	printf("%s", data);
	break;
      }
      default:
	return lbm_enc_sym(SYM_NIL);
	break;
      }
    } else if (val_type(t) == LBM_VAL_TYPE_CHAR) {
      printf("%c", lbm_dec_char(t));
    } else {
      int print_ret = lbm_print_value(output, 1024, error, 1024, t);

      if (print_ret >= 0) {
	printf("%s", output);
      } else {
	printf("%s", error);
      }
    }

  }
  return lbm_enc_sym(SYM_TRUE);
}


int output_arg_assembly(lbm_value arg) {

  lbm_value name;
  lbm_value num;

  switch (lbm_type_of(arg)) {
  case LBM_VAL_TYPE_I:
    //printf("outputing int argument\n");
    fprintf(out_file,"%d", lbm_dec_i(arg));
    break;
  case LBM_VAL_TYPE_U:
    //printf("outputing uint argument\n");
    fprintf(out_file,"%u", lbm_dec_u(arg));
    break;

  case LBM_PTR_TYPE_BOXED_I:
    fprintf(out_file,"%d", lbm_dec_I(arg));
    break;
  case LBM_PTR_TYPE_BOXED_U:
    fprintf(out_file,"%u", lbm_dec_U(arg));
    break;
  case LBM_PTR_TYPE_BOXED_F:
    fprintf(out_file,"%f", dec_f(arg));
    break;
    /* SYMBOL INDIRECTION */
  case PTR_TYPE_SYMBOL_INDIRECTION: {
    lbm_uint v = dec_symbol_indirection(arg);
    fprintf(out_file,"*%"PRI_UINT"*", v);
    break;
  }
    /* SYMBOL */
  case LBM_VAL_TYPE_SYMBOL: {
    const char *sym_name = lbm_get_name_by_symbol(lbm_dec_sym(arg));
    if (sym_name) {
      fprintf(out_file,"%s", sym_name);
    } else {
      printf("Error looking up symbol name\n");
      return 0;
    }
    break;
  }
    /* STRING */
  case LBM_PTR_TYPE_ARRAY: {
    lbm_array_header_t *array = (lbm_array_header_t *)arg;
    switch (array->elt_type){
    case LBM_VAL_TYPE_CHAR: {
      char *data = (char *)array + 8;
      fprintf(out_file,"%s", data);
      break;
    }
    default:
      printf("Error outputing string argument\n");
      return 0;
      break;
    }
    break;
  }

    /* LABEL */
  case LBM_PTR_TYPE_CONS: {
    name = lbm_car(lbm_cdr(arg));
    num  = lbm_car(lbm_cdr(lbm_cdr(arg)));
    if (lbm_type_of(name) == LBM_PTR_TYPE_ARRAY &&
	lbm_type_of(num)  == LBM_VAL_TYPE_I) {
      lbm_array_header_t *array = (lbm_array_header_t *)(lbm_car(name));
      switch (array->elt_type){
      case LBM_VAL_TYPE_CHAR: {
	char *data = (char *)array + 8;
	fprintf(out_file,"%s%d", data, lbm_dec_i(num));
	break;
      }
      default:
	printf("Error outputing label argument\n");
	return 0;
	break;
      }
    }
    break;
  }
  default:
    printf("Error default\n");
    return 0;
  }
  return 1;
}

/* ext_output_assembly
   args: label (as string, num) or Nil, (instr with args list)
*/
lbm_value ext_output_assembly(lbm_value *args, lbm_uint argn) {

  if (argn != 2)  return lbm_enc_sym(SYM_EERROR);

  /* Print potential label */
  if (lbm_type_of(args[0]) == LBM_PTR_TYPE_CONS) {
    lbm_value name = lbm_car(lbm_cdr(args[0]));
    lbm_value num  = lbm_car(lbm_cdr(lbm_cdr(args[0])));
    if (lbm_type_of(name) == LBM_PTR_TYPE_ARRAY &&
	lbm_type_of(num)  == LBM_VAL_TYPE_I) {
      lbm_array_header_t *array = (lbm_array_header_t *)(lbm_car(name));
      switch (array->elt_type){
      case LBM_VAL_TYPE_CHAR: {
	char *data = (char *)array + 8;
	char composite[1024];
	snprintf(composite, 1024, "%s%d", data, lbm_dec_i(num));
	fprintf(out_file,"%-20s", composite);
	break;
      }
      default:
	printf("Error in asm-out 1\n");
	return lbm_enc_sym(SYM_EERROR);
	break;
      }
    }
  } else if (lbm_type_of(args[0]) == LBM_VAL_TYPE_SYMBOL &&
	     lbm_dec_sym(args[0]) == SYM_NIL) {
    fprintf(out_file,"%-20s", "");
  }

  /* Print instruction opcode and potential arguments */

  if (lbm_type_of(args[1]) == LBM_PTR_TYPE_CONS) {
    lbm_value op_args = args[1];

    /* Try to print the instr and argument list */
    lbm_value curr = op_args;
    while (lbm_type_of(curr) != LBM_VAL_TYPE_SYMBOL) {
      if (!output_arg_assembly(lbm_car(curr))) {
	printf("Error in asm-out (argument output)\n");
	return lbm_enc_sym(SYM_EERROR);
      }
      fprintf(out_file,"\t");
      curr = lbm_cdr(curr);
    }
  } else {
    printf("Error in asm-out 3\n");
    return lbm_enc_sym(SYM_EERROR);
  }
  fprintf(out_file,"\n");
  return lbm_enc_sym(SYM_NIL);
}

/* ext_output_bytecode
   args: opcode, arguments
*/
lbm_value ext_output_bytecode(lbm_value *args, lbm_uint argn) {

  return lbm_enc_sym(SYM_NIL);
}

lbm_value ext_output_symbol_indirection(lbm_value *args, lbm_uint argn) {

  if (argn != 1) {
    printf("Error: Incorrect arguments to output_symbol_indirection\n"); 
    return lbm_enc_sym(SYM_EERROR);
  }
  
  if (lbm_type_of(args[0]) == LBM_PTR_TYPE_CONS) {
    lbm_value name = lbm_car(lbm_car(args[0]));
    lbm_value num  = lbm_cdr(lbm_car(args[0]));
    if (lbm_type_of(name) == LBM_PTR_TYPE_ARRAY) {
      lbm_array_header_t *array = (lbm_array_header_t *)(lbm_car(name));
      switch (array->elt_type){
      case LBM_VAL_TYPE_CHAR: {
	char *data = (char *)array + 8;
	char composite[1024];
	lbm_uint v = dec_symbol_indirection(num);
	snprintf(composite, 1024,"*%"PRI_UINT"* %s %u\n",v,  data, num); /*raw */
	fprintf(out_file,"%s", composite);
	break;
      }
      default:
	printf("Error: Incorrect argument type to output_symbol_indirection\n");
	return lbm_enc_sym(SYM_EERROR);
	break;
      }
    } else {
      printf("Error: Incorrect argument type to output_symbol_indirection\n");
    }
  } else {
    printf("Error: Incorrect argument type to output_symbol_indirection\n");
  }
  return lbm_enc_sym(SYM_NIL);
}

/* load a file, caller is responsible for freeing the returned string */
char * load_file(FILE *fp) {
  char *file_str = NULL;

  long fsize_long;
  unsigned int fsize;
  fseek(fp, 0, SEEK_END);
  fsize_long = ftell(fp);
  if (fsize_long <= 0) {
    printf("File is empty\n");
    return NULL;
  }
  fsize = (unsigned int) fsize_long;
  fseek(fp, 0, SEEK_SET);
  file_str = malloc(fsize+1);
  memset(file_str, 0 , fsize+1);
  if (fread(file_str,1,fsize,fp) != fsize) {
    free(file_str);
    file_str = NULL;
  }
  fclose(fp);
  return file_str;
}


/* Arguments:
   first non-option argument is input file
   -o output-file
   -S out assembler
 */

char input_file[1024];
char output_file[1024];
bool output_file_ok = false;
bool output_assembler = false;

int parse_args(int argc, char **argv) {
  int c;

  memset(input_file,0,1024);
  memset(output_file,0,1024);

  while (1) {
    int option_index = 0;
    static struct option long_options[] = {
      {0,         0,                 0,  0 }
    };

    c = getopt_long(argc, argv, "So:",
		    long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {

    case 'S':
      output_assembler = true;
      break;

    case 'o':
      if (strlen(optarg) < 1023) {
	strncpy(output_file, optarg, 1024);
	output_file_ok = true;
      } else {
	printf("Error: Output filename is too long\n");
	return 0;
      }
      break;

    case '?':
      break;

    default:
      printf("?? getopt returned character code 0%o ??\n", c);
    }
  }

  if (optind < argc) {
    if (strlen(argv[optind]) >= 1023) {
      printf("Error: Input filename is too long\n");
      return 0;
    }

    strncpy(input_file, argv[optind], 1024);

    bool dot_found = false;
    size_t dot_index = strlen(input_file);
    while (dot_index >= 0)  {
      if (input_file[dot_index] == '.') {
	dot_found = true;
	break;
      }
      dot_index --;
    }

    if (!dot_found) {
      printf("Error: Incorrect input filename %s. File extension missing\n", input_file);
      return 0;
    }

    if (!output_file_ok) {
      strncpy(output_file,input_file,1024);
      output_file[dot_index] = 0;
      if (dot_index < 1020) {
	if (output_assembler) {
	  output_file[dot_index]   = '.';
	  output_file[dot_index+1] = 'S';
	  output_file[dot_index+2] = 0;
	} else {
	  output_file[dot_index]   = '.';
	  output_file[dot_index+1] = 'b';
	  output_file[dot_index+2] = 'm';
	  output_file[dot_index+3] = 'c';
	  output_file[dot_index+4] = 0;
	}
      } else {
	printf("Error: Input filename is too long\n");
	return 0;
      }

      output_file_ok = true;
    }

    printf("Output to: %s\n", output_file);

  } else {
    printf("Error: No input file specified\n");
    return 0;
  }


  /* Attempt open input and output files */
  in_file = fopen(input_file, "r");

  if (!in_file) {
    printf("Error: Unable to open input file %s\n", input_file);
    return 0;
  }

  out_file = fopen(output_file, "w");

  if (!out_file) {
    printf("Error: Unable to open output file %s\n", output_file);
    return 0;
  }


  return 1;
}


int main(int argc, char **argv) {
  int res = 0;

  if (!parse_args(argc, argv))
    exit(EXIT_FAILURE);

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


  res = lbm_heap_init(heap, HEAP_SIZE);
  if (res)
    printf("Heap initialized. Heap size: %f MiB. Free cons cells: %d\n", lbm_heap_size_bytes() / 1024.0 / 1024.0, lbm_heap_num_free());
  else {
    printf("Error initializing heap!\n");
    return 0;
  }

  res = lbm_init_env();
  if (res)
    printf("Environment initialized.\n");
  else {
    printf("Error initializing environment!\n");
    return 0;
  }

  res = lbm_add_extension("print", ext_print);
  if (res)
    printf("Extension print added.\n");
  else {
    printf("Error adding print extension.\n");
    return 0;
  }

  res = lbm_add_extension("asm-out", ext_output_assembly);
  if (res)
    printf("Extension asm-out added.\n");
  else {
    printf("Error adding asm-out extension.\n");
    return 0;
  }

  res = lbm_add_extension("ind-out", ext_output_symbol_indirection);
  if (res)
    printf("Extension ind-out added.\n");
  else {
    printf("Error adding ind-out extension.\n");
    return 0;
  }

  

  char output[1024];
  char error[1024];

  lbm_value prelude = prelude_load();
  lbm_value p_r = ec_eval_program(prelude);

  int r = lbm_print_value(output, 1024, error, 1024, p_r);

  if (lbm_is_symbol(p_r) && lbm_is_error(lbm_dec_sym(p_r))) {
    printf("Error loading Prelude: %s\n", r == 0 ? "UNKNOWN" : output);
  } else {
    printf("Prelude loaded successfully: %s\n", r == 0 ? "UNKNOWN" : output);
  }

  FILE *fp;
  fp = fopen("compile.lisp", "r");
  if (!fp) {
    printf("Error: Unable to open compile.lisp\n");
    return 1;
  }

  char *comp_str = load_file(fp);
  lbm_value f_exp = tokpar_parse(comp_str);
  free(comp_str);
  lbm_value c_r = ec_eval_program(f_exp);

  r = lbm_print_value(output, 1024, error, 1024, c_r);

  if (lbm_is_symbol(c_r) && lbm_is_error(lbm_dec_sym(c_r))) {
    printf("Error loading compiler: %s\n", r == 0 ? "UNKNOWN" : "output");
  } else {
    printf("Compiler loaded successfully: %s\n", r == 0 ? "UNKNOWN" : output);
  }

  char *file_str = load_file(in_file);
  lbm_value input_prg = tokpar_parse(file_str);

  free(file_str);

  lbm_uint compiler;
  if (lbm_get_symbol_by_name("gen-asm", &compiler)) {
    lbm_value invoce_compiler = lbm_cons(lbm_cons (lbm_enc_sym(compiler),
				       lbm_cons(lbm_cons (lbm_enc_sym(SYM_QUOTE),
						  lbm_cons (input_prg, lbm_enc_sym(SYM_NIL))),
					    lbm_enc_sym(SYM_NIL))),
				 lbm_enc_sym(SYM_NIL));

    r = lbm_print_value(output, 1024, error, 1024, invoce_compiler);
    if (r >= 0) {
      printf("> %s\n", output );
    } else {
      printf("%s\n", error);
    }


    lbm_value compiled_res = ec_eval_program(invoce_compiler);

    r = lbm_print_value(output, 1024, error, 1024, compiled_res);
    if (r >= 0) {
      printf("> %s\n", output );
    } else {
      printf("%s\n", error);
    }
  } else {
    printf("Error: Compiler not present\n");
  }

  symrepr_del();

  return 0;
}
