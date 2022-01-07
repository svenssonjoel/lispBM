/*
    Copyright 2018, 2021 Joel Svensson  svenssonjoel@yahoo.se

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
#include <termios.h>
#include <ctype.h>

#include "heap.h"
#include "symrepr.h"
#include "extensions.h"
#include "eval_cps.h"
#include "print.h"
#include "tokpar.h"
#include "prelude.h"
#include "typedefs.h"
#include "memory.h"
#include "env.h"

#define EVAL_CPS_STACK_SIZE 256

static volatile bool allow_print = true;

struct termios old_termios;
struct termios new_termios;

void setup_terminal(void) {

  tcgetattr(0,&old_termios);
  new_termios = old_termios;
  //new_termios.c_iflag;                     // INPUT MODES
  //new_termios.c_oflag;                     // OUTPUT MODES
  //new_termios.c_cflag;                     // CONTROL MODES
  // LOCAL MODES
  new_termios.c_lflag &= (tcflag_t) ~(ICANON  | ISIG | ECHO);
  new_termios.c_cc[VMIN] = 0;
  new_termios.c_cc[VTIME] = 0;
  //new_termios.c_cc;                       // SPECIAL CHARACTERS

  // LOCAL MODES
  // Turn off:
  //  - canonical mode
  //  - Signal generation for certain characters (INTR, QUIT, SUSP, DSUSP)
  //  VMIN:  Minimal number of characters for noncanonical read.
  //  VTIME: Timeout in deciseconds for noncanonical read.

  tcsetattr(0, TCSANOW, &new_termios);

}

void restore_terminal(void) {
  tcsetattr(0, TCSANOW, &old_termios);
}


int inputline(char *buffer, unsigned int size) {
  int n = 0;
  int c;
  for (n = 0; n < size - 1; n++) {

    c = getchar(); // busy waiting.

    if (c < 0) {
      n--;
      struct timespec s;
      struct timespec r;
      s.tv_sec = 0;
      s.tv_nsec = (long)1000 * 1000;
      nanosleep(&s, &r);
      continue;
    }
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      //putchar(0x8); /* output backspace character */
      //putchar(' ');
      //putchar(0x8);
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        //putchar(c);
        buffer[n] = (char)c;
      } else {
        n -= 1;
      }
      break;
    }
  }
  buffer[size - 1] = 0;
  return 0; // Filled up buffer without reading a linebreak
}

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
    printf("<< Context %d finished with value %s >>\n", cid, output);
  } else {
    printf("<< Context %d finished with value %s >>\n", cid, error);
  }

  //  if (!eval_cps_remove_done_ctx(cid, &t)) {
  //   printf("Error: done context (%d)  not in list\n", cid);
  //}
  fflush(stdout);

}

uint32_t timestamp_callback() {
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return (uint32_t)(tv.tv_sec * 1000000 + tv.tv_usec);
}

void sleep_callback(uint32_t us) {
  struct timespec s;
  struct timespec r;
  s.tv_sec = 0;
  s.tv_nsec = (long)us * 1000;
  nanosleep(&s, &r);
}


VALUE ext_print(VALUE *args, UINT argn) {
  if (argn < 1) return enc_sym(SYM_NIL);

  if (!allow_print) return enc_sym(SYM_TRUE);

  char output[1024];
  char error[1024];

  for (int i = 0; i < argn; i ++) {
    VALUE t = args[i];

    if (is_ptr(t) && ptr_type(t) == PTR_TYPE_ARRAY) {
      array_header_t *array = (array_header_t *)car(t);
      switch (array->elt_type){
      case VAL_TYPE_CHAR: {
        char *data = (char*)array + 8;
        printf("%s", data);
        break;
      }
      default:
        return enc_sym(SYM_NIL);
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
  return enc_sym(SYM_TRUE);
}

VALUE ext_note(VALUE *args, UINT argn) {

  if (argn != 2) {
    return enc_sym(SYM_NIL);
  }

  printf("note %u %u\n", dec_u(args[0]),  dec_u(args[1]));

  return enc_sym(SYM_TRUE);
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


void print_ctx_info(eval_context_t *ctx, void *arg1, void *arg2) {
  (void) arg1;
  (void) arg2;

  char output[1024];
  char error[1024];

  int print_ret = print_value(output, 1024, error, 1024, ctx->r);

  printf("--------------------------------\n");
  printf("ContextID: %u\n", ctx->id);
  printf("Stack SP: %u\n",  ctx->K.sp);
  if (print_ret) {
    printf("Value: %s\n", output);
  } else {
    printf("Error: %s\n", error);
  }
}

void ctx_exists(eval_context_t *ctx, void *arg1, void *arg2) {

  CID id = *(CID*)arg1;
  bool *exists = (bool*)arg2;

  if (ctx->id == id) {
    *exists = true;
  }
}


int main(int argc, char **argv) {
  char *str = malloc(1024);;
  unsigned int len = 1024;
  int res = 0;

  pthread_t lispbm_thd;

  heap_state_t heap_state;

  cons_t *heap_storage = NULL;

  //setup_terminal();

  unsigned char *memory = malloc(MEMORY_SIZE_8K);
  unsigned char *bitmap = malloc(MEMORY_BITMAP_SIZE_8K);
  if (memory == NULL || bitmap == NULL) return 0;

  res = memory_init(memory, MEMORY_SIZE_8K,
                    bitmap, MEMORY_BITMAP_SIZE_8K);
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

  unsigned int heap_size = 2048;

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

  res = extensions_add("note", ext_note);
  if (res)
    printf("Extension added.\n");
  else
    printf("Error adding extension.\n");


  /* Start evaluator thread */
  if (pthread_create(&lispbm_thd, NULL, eval_thd_wrapper, NULL)) {
    printf("Error creating evaluation thread\n");
    return 1;
  }

  VALUE prelude = prelude_load();
  eval_cps_program(prelude);

  printf("Lisp REPL started!\n");
  printf("Type :quit to exit.\n");
  printf("     :info for statistics.\n");
  printf("     :load [filename] to load lisp source.\n");

  char output[1024];
  char error[1024];

  while (1) {
    fflush(stdin);
    printf("# ");
    memset(str, 0 ,len);

    ssize_t n = inputline(str,len);
    fflush(stdout);
    //printf("\n");

    if (n >= 5 && strncmp(str, ":info", 5) == 0) {
      printf("############################################################\n");
      printf("Used cons cells: %d\n", heap_size - heap_num_free());
      int r = print_value(output, 1024, error, 1024, *env_get_global_ptr());
      if (r >= 0) {
        printf("ENV: %s\n", output );
      } else {
        printf("%s\n", error);
      }
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
      char *file_str = load_file(&str[5]);
      if (file_str) {

        /* Get exclusive access to the heap */
        eval_cps_pause_eval();
        while(eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
          sleep_callback(10);
        }

        VALUE f_exp = tokpar_parse(file_str);
        free(file_str);
        CID cid1 = eval_cps_program(f_exp);

        eval_cps_continue_eval();

        printf("started ctx: %u\n", cid1);
      }
    } else if (n >= 4 && strncmp(str, ":pon", 4) == 0) {
      allow_print = true;
      continue;
    } else if (n >= 5 && strncmp(str, ":poff", 5) == 0) {
      allow_print = false;
      continue;
    } else if (strncmp(str, ":ctxs", 5) == 0) {
      printf("****** Running contexts ******\n");
      eval_cps_running_iterator(print_ctx_info, NULL, NULL);
      printf("****** Blocked contexts ******\n");
      eval_cps_blocked_iterator(print_ctx_info, NULL, NULL);
      printf("****** Done contexts ******\n");
      eval_cps_done_iterator(print_ctx_info, NULL, NULL);
    } else if (strncmp(str, ":wait", 5) == 0) {
      int id = atoi(str + 5);
      bool exists = false;
      eval_cps_done_iterator(ctx_exists, (void*)&id, (void*)&exists);
      if (exists) {
        eval_cps_wait_ctx((CID)id);
      }
    }
    else if (n >= 5 && strncmp(str, ":quit", 5) == 0) {
      break;
    } else {
      VALUE t;

      /* Get exclusive access to the heap */
      eval_cps_pause_eval();
      while(eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }

      t = tokpar_parse(str);

      CID cid = eval_cps_program(t);

      eval_cps_continue_eval();

      printf("started ctx: %u\n", cid);

    }
  }

  symrepr_del();
  free(heap_storage);

  //restore_terminal();

  return 0;
}
