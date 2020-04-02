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
#include <errno.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>

#include "heap.h"
#include "symrepr.h"
#include "extensions.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "typedefs.h"

#define EVAL_CPS_STACK_SIZE 256

void *eval_thd_wrapper(void *v) {

  eval_cps_run_eval();
  
  return NULL;
}

void done_callback(eval_context_t *ctx) {

  char output[1024];
  char error[1024];

  CID cid = ctx->id;
  VALUE t = ctx->r;
  
  int print_ret = print_value(output, 1024, error, 1024, t);

  if (print_ret >= 0) {
    printf("<< Context %d finished with value %s >>\n# ", cid, output);
  } else {
    printf("<< Context %d finished with value %s >>\n# ", cid, error);
  }
  fflush(stdout);
}

uint32_t timestamp_callback() {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (tv.tv_sec * 1000000 + (uint32_t)tv.tv_usec);
}

void sleep_callback(uint32_t us) {
  struct timespec s;
  struct timespec r;
  s.tv_sec = 0;
  s.tv_nsec = us * 1000;
  nanosleep(&s, &r);
}


VALUE ext_print(VALUE *args, int argn) {
  if (argn < 1) return enc_sym(symrepr_nil());

  char output[1024];
  char error[1024];
  
  for (int i = 0; i < argn; i ++) {
    VALUE t = args[i];

    if (is_ptr(t) && ptr_type(t) == PTR_TYPE_ARRAY) {
      array_t *array = (array_t *)car(t);
      switch (array->elt_type){
      case VAL_TYPE_CHAR:
	printf("%s", array->data.c);
	break;
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
  unsigned int str_len = strlen(filename);
  filename[str_len-1] = 0;
  int i = 0;
  while (filename[i] == ' ' && filename[i] != 0) {
    i ++;
  }
  FILE *fp;
  printf("filename: %s\n", &filename[i]);
	
  if (strlen(&filename[i]) > 0) {
    errno = 0;
    fp = fopen(&filename[i], "r");
    if (!fp) {
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

int main(int argc, char **argv) {
  char *str = malloc(1024);;
  size_t len = 1024;
  int res = 0;

  pthread_t lispbm_thd; 

  heap_state_t heap_state;

  res = symrepr_init();
  if (res)
    printf("Symrepr initialized.\n");
  else {
    printf("Error initializing symrepr!\n");
    return 0;
  }

  unsigned int heap_size = 2048;
  res = heap_init(heap_size);
  if (res)
    printf("Heap initialized. Heap size: %f MiB. Free cons cells: %d\n", heap_size_bytes() / 1024.0 / 1024.0, heap_num_free());
  else {
    printf("Error initializing heap!\n");
    return 0;
  }

  res = eval_cps_init(); // dont grow stack 
  if (res)
    printf("Evaluator initialized.\n");
  else {
    printf("Error initializing evaluator.\n");
  }

  eval_cps_set_ctx_done_callback(done_callback);
  eval_cps_set_timestamp_us_callback(timestamp_callback);
  eval_cps_set_usleep_callback(sleep_callback);
  
  
  res = extensions_add("print", ext_print);
  if (res)
    printf("Extension added.\n");
  else
    printf("Error adding extension.\n");

  VALUE prelude = prelude_load();
  eval_cps_program(prelude);

  printf("Lisp REPL started!\n");
  printf("Type :quit to exit.\n");
  printf("     :info for statistics.\n");
  printf("     :load [filename] to load lisp source.\n");

  char output[1024];
  char error[1024];

  /* Start evaluator thread */
  if (pthread_create(&lispbm_thd, NULL, eval_thd_wrapper, NULL)) {
    printf("Error creating evaluation thread\n");
    return 1;
  }

 
  
  while (1) {
    fflush(stdin);
    printf("# ");
    memset(str, 0 ,len);
    ssize_t n = getline(&str,&len,stdin);

    if (n >= 5 && strncmp(str, ":info", 5) == 0) {
      printf("############################################################\n");
      printf("Used cons cells: %d\n", heap_size - heap_num_free());
      int r = print_value(output, 1024, error, 1024, eval_cps_get_env());
      if (r >= 0) { 
	printf("ENV: %s\n", output );
      } else {
	printf("%s\n", error);
      }
      heap_get_state(&heap_state);
      printf("Allocated arrays: %u\n", heap_state.num_alloc_arrays);
      printf("GC counter: %d\n", heap_state.gc_num);
      printf("Recovered: %d\n", heap_state.gc_recovered);
      printf("Recovered arrays: %u\n", heap_state.gc_recovered_arrays);
      printf("Marked: %d\n", heap_state.gc_marked);
      printf("Free cons cells: %d\n", heap_num_free());
      printf("############################################################\n");
    } else if (n >= 5 && strncmp(str, ":load", 5) == 0) {
      char *file_str = load_file(&str[5]);
      if (file_str) {
	VALUE f_exp = tokpar_parse(file_str);
	free(file_str);
	CID cid1 = eval_cps_program(f_exp);
	printf("started ctx: %u\n", cid1);
      } 
    } else  if (n >= 5 && strncmp(str, ":quit", 5) == 0) {
      break;
    } else {

      VALUE t;
      t = tokpar_parse(str);

      CID cid = eval_cps_program(t);

      printf("started ctx: %u\n", cid);

    }
  }

  symrepr_del();
  heap_del();

  return 0;
}
