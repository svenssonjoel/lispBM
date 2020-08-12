/*
    Copyright 2018, 2020 Joel Svensson	svenssonjoel@yahoo.se

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

VALUE ext_print(VALUE *args, int argn) {
  if (argn < 1) return enc_sym(symrepr_nil());

  char output[1024];
  char error[1024];

  for (int i = 0; i < argn; i ++) {
    VALUE t = args[i];

    if (is_ptr(t) && ptr_type(t) == PTR_TYPE_ARRAY) {
      array_header_t *array = (array_header_t *)car(t);
      switch (array->elt_type){
      case VAL_TYPE_CHAR: {
	char *data = (char *)array + 8;
	printf("%s", data);
	break;
      }
      default:
	return enc_sym(symrepr_nil());
	break;
      }
    } else if (val_type(t) == VAL_TYPE_CHAR) {
      printf("%c", dec_char(t));
    } else {
      int print_ret = print_value(output, 1024, error, 1024, t);

      if (print_ret >= 0) {
	printf("%s", output);
      } else {
	printf("%s", error);
      }
    }

  }
  return enc_sym(symrepr_true());
}

/* load a file, caller is responsible for freeing the returned string */
char * load_file(char *filename) {
  char *file_str = NULL;
  //size_t str_len = strlen(filename);
  //filename[str_len-1] = 0;
  int i = 0;
  while (filename[i] == ' ' && filename[i] != 0) {
    i ++;
  }
  FILE *fp;

  if (strlen(&filename[i]) > 0) {
    errno = 0;
    fp = fopen(&filename[i], "r");
    if (!fp) {
      printf("cannot fopen file %s\n", &filename[i]);
      printf("filename length: %d\n", strlen(&filename[i]));
      printf("%d\n", errno);
      return NULL;
    }
    long fsize_long;
    unsigned int fsize;
    fseek(fp, 0, SEEK_END);
    fsize_long = ftell(fp);
    if (fsize_long <= 0) {
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
  }
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
  int digit_optind = 0;

  memset(input_file,0,1024);
  memset(output_file,0,1024);
  
  while (1) {
    int this_option_optind = optind ? optind : 1;
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
    int dot_index = strlen(input_file);
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

  return 1;
}


int main(int argc, char **argv) {
  char *str = malloc(1024);;
  unsigned int len = 1024;
  int res = 0;

  // Experiment
  if (!parse_args(argc, argv))
    exit(EXIT_FAILURE);
  // Experiment

  heap_state_t heap_state;

  unsigned char *memory = malloc(MEMORY_SIZE_16K);
  unsigned char *bitmap = malloc(MEMORY_BITMAP_SIZE_16K);
  if (memory == NULL || bitmap == NULL) return 0;

  res = memory_init(memory, MEMORY_SIZE_16K,
		    bitmap, MEMORY_BITMAP_SIZE_16K);
  if (res)
    printf("Memory initialized. Memory size: %u Words. Free: %u Words.\n", memory_num_words(), memory_num_free());
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

  unsigned int heap_size = 8192;
  res = heap_init(heap_size);
  if (res)
    printf("Heap initialized. Heap size: %f MiB. Free cons cells: %d\n", heap_size_bytes() / 1024.0 / 1024.0, heap_num_free());
  else {
    printf("Error initializing heap!\n");
    return 0;
  }

  res = env_init();
  if (res)
    printf("Environment initialized.\n");
  else {
    printf("Error initializing environment!\n");
    return 0;
  }

  res = extensions_add("print", ext_print);
  if (res)
    printf("Extension added.\n");
  else {
    printf("Error adding extension.\n");
    return 0;
  }

  char output[1024];
  char error[1024];
  
  VALUE prelude = prelude_load();
  VALUE p_r = ec_eval_program(prelude);

  int r = print_value(output, 1024, error, 1024, p_r);
  
  if (is_symbol(p_r) && symrepr_is_error(dec_sym(p_r))) {
    printf("Error loading Prelude: %s\n", r == 0 ? "UNKNOWN" : output);
  } else {
    printf("Prelude loaded successfully: %s\n", r == 0 ? "UNKNOWN" : output);
  }

  char *comp_str = load_file("compile.lisp");
  VALUE f_exp = tokpar_parse(comp_str);
  free(comp_str);
  VALUE c_r = ec_eval_program(f_exp);

  r = print_value(output, 1024, error, 1024, c_r);
  
  if (is_symbol(c_r) && symrepr_is_error(dec_sym(c_r))) {
    printf("Error loading compiler: %s\n", r == 0 ? "UNKNOWN" : "output");
  } else {
    printf("Compiler loaded successfully: %s\n", r == 0 ? "UNKNOWN" : output);
  }

  printf("Lisp REPL started!\n");
  printf("Type :quit to exit.\n");
  printf("     :info for statistics.\n");
  printf("     :load [filename] to load lisp source.\n");
  
  while (1) {
    fflush(stdin);
    printf("# ");
    memset(str, 0 ,len);

    ssize_t n = getline(&str,&len, stdin);
    printf("\n");

    if (n >= 5 && strncmp(str, ":info", 5) == 0) {
      printf("############################################################\n");
      printf("Used cons cells: %d\n", heap_size - heap_num_free());
      int r = print_value(output, 1024, error, 1024, *env_get_global_ptr());
      if (r >= 0) {
	printf("ENV: %s\n", output );
      } else {
	printf("%s\n", error);
      }
      int env_len = 0;
      VALUE curr = *env_get_global_ptr();
      while (type_of(curr) == PTR_TYPE_CONS) {
	env_len ++;
	curr = cdr(curr);
      }
      printf("Global env num bindings: %d\n", env_len);
      heap_get_state(&heap_state);
      printf("Symbol table size: %u Bytes\n", symrepr_size());
      printf("Heap size: %u Bytes\n", heap_size * 8);
      printf("Memory size: %u Words\n", memory_num_words());
      printf("Memory free: %u Words\n", memory_num_free());
      printf("Allocated arrays: %u\n", heap_state.num_alloc_arrays);
      printf("GC counter: %d\n", heap_state.gc_num);
      printf("Recovered: %d\n", heap_state.gc_recovered);
      printf("Recovered arrays: %u\n", heap_state.gc_recovered_arrays);
      printf("Marked: %d\n", heap_state.gc_marked);
      printf("Free cons cells: %d\n", heap_num_free());
      printf("############################################################\n");
    } else if (n >= 5 && strncmp(str, ":load", 5) == 0) {
      unsigned int fstr_len = strlen(&str[5]);
      str[5+fstr_len-1] = 0;
      char *file_str = load_file(&str[5]);
      if (file_str) {
	printf("Loading file %s\n", &str[5]);
	VALUE f_exp = tokpar_parse(file_str);
	free(file_str);
	VALUE l_r = ec_eval_program(f_exp);
	int r = print_value(output, 1024, error, 1024, l_r);
	if (r >= 0) {
	  printf("> %s\n", output );
	} else {
	  printf("%s\n", error);
	}
      } else {
	printf("Failed to load file %s\n", &str[5]);
      }


    }  else if (n >= 5 && strncmp(str, ":quit", 5) == 0) {
      break;
    } else {

      VALUE t;
      t = tokpar_parse(str);

      VALUE r_r = ec_eval_program(t);

      int r = print_value(output, 1024, error, 1024, r_r);
      if (r >= 0) {
	printf("> %s\n", output );
      } else {
	printf("%s\n", error);
      }

    }
  }

  symrepr_del();
  heap_del();

  return 0;
}
