/*
    Copyright 2019, 2022  Joel Svensson        svenssonjoel@yahoo.se
                    2022  Benjamin Vedder

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
#include "ctype.h"

#include "ch.h"
#include "hal.h"
#include "chvt.h"
#include "chtime.h"

#include "usbcfg.h"
#include "chprintf.h"

#include "lispbm.h"

#define EVAL_WA_SIZE THD_WORKING_AREA_SIZE(1024)
#define EVAL_CPS_STACK_SIZE 256

#define HEAP_SIZE 2048

static cons_t heap[HEAP_SIZE] __attribute__ ((aligned (8)));

static uint32_t memory_array[MEMORY_SIZE_8K];
static uint32_t bitmap_array[MEMORY_BITMAP_SIZE_8K];

static tokenizer_string_state_t string_tok_state;
static tokenizer_char_stream_t string_tok;

BaseSequentialStream *chp = NULL;

int inputline(BaseSequentialStream *chp, char *buffer, int size) {
  int n = 0;
  unsigned char c;
  for (n = 0; n < size - 1; n++) {

    c = streamGet(chp);
    switch (c) {
    case 127: /* fall through to below */
    case '\b': /* backspace character received */
      if (n > 0)
        n--;
      buffer[n] = 0;
      streamPut(chp,0x8); /* output backspace character */
      streamPut(chp,' ');
      streamPut(chp,0x8);
      n--; /* set up next iteration to deal with preceding char location */
      break;
    case '\n': /* fall through to \r */
    case '\r':
      buffer[n] = 0;
      return n;
    default:
      if (isprint(c)) { /* ignore non-printable characters */
        streamPut(chp,c);
        buffer[n] = c;
      } else {
        n -= 1;
      }
      break;
    }
  }
  buffer[size - 1] = 0;
  return 0; // Filled up buffer without reading a linebreak
}

static char print_output[1024];

void done_callback(eval_context_t *ctx) {

  char *output = print_output;

  CID cid = ctx->id;
  VALUE t = ctx->r;

  int print_ret = print_value(output, 1024, t);

  if (print_ret >= 0) {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, output);
  } else {
    chprintf(chp,"<< Context %d finished with value %s >>\r\n# ", cid, output);
  }
}

uint32_t timestamp_callback(void) {
  systime_t t = chVTGetSystemTime();
  return (uint32_t) (100 * t);
}

void sleep_callback(uint32_t us) {
  chThdSleepMicroseconds(us);
}

static THD_FUNCTION(eval, arg) {
  (void) arg;
  eval_cps_run_eval();
}

/* ext_print is atomic from the point of view of the lisp RTS */
VALUE ext_print(VALUE *args, UINT argn) {

  char *output = print_output;

  for (UINT i = 0; i < argn; i ++) {
    VALUE t = args[i];

    if (is_ptr(t) && ptr_type(t) == PTR_TYPE_ARRAY) {
      array_header_t *array = (array_header_t *)car(t);
      switch (array->elt_type){
      case VAL_TYPE_CHAR:
        chprintf(chp,"%s", (char*)array + 8);
        break;
      default:
        return enc_sym(SYM_NIL);
        break;
      }
    } else if (val_type(t) == VAL_TYPE_CHAR) {
      if (dec_char(t) =='\n') {
        chprintf(chp, "\r\n");
      } else {
        chprintf(chp,"%c", dec_char(t));
      }
    }  else {
      print_value(output, 1024, t);
      chprintf(chp,"%s", output);
    }
  }
  return enc_sym(SYM_TRUE);
}

static char str[1024];
static char outbuf[1024];
static char file_buffer[2048];

void print_ctx_info(eval_context_t *ctx, void *arg1, void *arg2) {
  (void)arg2;
  print_value(outbuf, 1024, ctx->r);
  chprintf(chp, "%s %x %u %u %s\r\n", (char*)arg1, (uint32_t)ctx, ctx->id, ctx->K.sp, outbuf);
}

void ctx_exists(eval_context_t *ctx, void *arg1, void *arg2) {

  CID id = *(CID*)arg1;
  bool *exists = (bool*)arg2;

  if (ctx->id == id) {
    *exists = true;
  }
}
int main(void) {
  halInit();
  chSysInit();

  sduObjectInit(&SDU1);
  sduStart(&SDU1, &serusbcfg);

  /*
   * Activates the USB driver and then the USB bus pull-up on D+.
   * Note, a delay is inserted in order to not have to disconnect the cable
   * after a reset.
   */
  usbDisconnectBus(serusbcfg.usbp);
  chThdSleepMilliseconds(1500);
  usbStart(serusbcfg.usbp, &usbcfg);
  usbConnectBus(serusbcfg.usbp);

  chp = (BaseSequentialStream*)&SDU1;

  size_t len = 1024;

  int res = 0;

  heap_state_t heap_state;

  chThdSleepMilliseconds(2000);

  if (!lispbm_init(heap, HEAP_SIZE,
                   memory_array, MEMORY_SIZE_8K,
                   bitmap_array, MEMORY_BITMAP_SIZE_8K)) {
    chprintf(chp,"LispBM Init failed.\r\n");
    return 0;
  }

  eval_cps_set_ctx_done_callback(done_callback);
  eval_cps_set_timestamp_us_callback(timestamp_callback);
  eval_cps_set_usleep_callback(sleep_callback);

  res = extensions_add("print", ext_print);
  if (res)
    chprintf(chp,"Extension added.\r\n");
  else
    chprintf(chp,"Error adding extension.\r\n");

  thread_t *t = chThdCreateFromHeap(NULL, EVAL_WA_SIZE,
                                    "eval", NORMALPRIO+1,
                                    eval, (void *)NULL);

  if (!t) {
    chprintf(chp,"Error starting evaluator thread.\r\n");
    return 0;
  }
  chprintf(chp,"Lisp REPL started (ChibiOS)!\r\n");

  while (1) {
    chprintf(chp,"# ");
    memset(str,0,len);
    memset(outbuf,0, 1024);
    inputline(chp,str, len);
    chprintf(chp,"\r\n");

    if (strncmp(str, ":info", 5) == 0) {
      chprintf(chp,"------------------------------------------------------------\r\n");
      chprintf(chp,"Used cons cells: %lu \r\n", HEAP_SIZE - heap_num_free());
      chprintf(chp,"Free cons cells: %lu\r\n", heap_num_free());
      heap_get_state(&heap_state);
      chprintf(chp,"GC counter: %lu\r\n", heap_state.gc_num);
      chprintf(chp,"Recovered: %lu\r\n", heap_state.gc_recovered);
      chprintf(chp,"Marked: %lu\r\n", heap_state.gc_marked);

      chprintf(chp,"Array and symbol string memory:\r\n");
      chprintf(chp,"  Size: %u 32Bit words\r\n", memory_num_words());
      chprintf(chp,"  Free: %u 32Bit words\r\n", memory_num_free());
      chprintf(chp,"------------------------------------------------------------\r\n");
      memset(outbuf,0, 1024);
    } else if (strncmp(str, ":env", 4) == 0) {
      VALUE curr = *env_get_global_ptr();
      chprintf(chp,"Environment:\r\n");
      while (type_of(curr) == PTR_TYPE_CONS) {
        res = print_value(outbuf,1024, car(curr));
        curr = cdr(curr);

        chprintf(chp,"  %s \r\n", outbuf);
      }
    } else if (strncmp(str, ":threads", 8) == 0) {
      thread_t *tp;
      static const char *states[] = {CH_STATE_NAMES};
      chprintf(chp, "    addr prio refs     state           name time    \r\n");
      chprintf(chp, "-------------------------------------------------------------------\r\n");
      tp = chRegFirstThread();
      do {
        chprintf(chp, "%.8lx %4lu %4lu %9s %14s %lu (%.1f %%)\r\n",
                 (uint32_t)tp,
                 (uint32_t)tp->prio, (uint32_t)(tp->refs - 1),
                 states[tp->state], tp->name, (uint32_t)(tp->time - tp->time_last),
                 (double)(100.0 * (float)(tp->time - tp->time_last) / (float)(chVTGetSystemTimeX() - tp->time_last)));
        tp->time_last = tp->time;
        tp = chRegNextThread(tp);
      } while (tp != NULL);
    } else if (strncmp(str, ":mem", 4) == 0) {
      size_t n, size, sizel;
      n = chHeapStatus(NULL, &size, &sizel);
      chprintf(chp, "core free memory  : %u bytes\r\n", chCoreGetStatusX());
      chprintf(chp, "heap fragments    : %u\r\n", n);
      chprintf(chp, "heap free largest : %u bytes\r\n", sizel);
      chprintf(chp, "heap free total   : %u bytes\n\r\n", size);
    } else if (strncmp(str, ":ctxs", 5) == 0) {
      eval_cps_running_iterator(print_ctx_info, "RUNNABLE", NULL);
      eval_cps_blocked_iterator(print_ctx_info, "BLOCKED", NULL);
      eval_cps_done_iterator   (print_ctx_info, "DONE", NULL);
    } else if (strncmp(str, ":wait", 5) == 0) {
      int id = atoi(str + 5);
      bool exists = false;
      eval_cps_done_iterator(ctx_exists, (void*)&id, (void*)&exists);
      if (exists) {
        eval_cps_wait_ctx((CID)id);
      }
    } else if (strncmp(str, ":pause", 6) == 0) {
      eval_cps_pause_eval();
      while(eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }
      chprintf(chp, "Evaluator paused\r\nEnter command :continue to unpause or :step to perform single stepping\r\n");
    } else if (strncmp(str, ":continue", 9) == 0) {
      eval_cps_continue_eval();
    } else if (strncmp(str, ":step", 5) == 0) {
      eval_cps_step_eval();
      while(eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
        chThdSleepMilliseconds(1);
      }
      chprintf(chp, "Evaluator paused\r\nEnter command :continue to unpause or :step to perform single stepping\r\n");
    } else if (strncmp(str, ":reset", 6) == 0) {
      eval_cps_pause_eval();
      while(eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
        chThdSleepMilliseconds(1);
      }

      lispbm_init(heap, HEAP_SIZE,
                  memory_array, MEMORY_SIZE_8K,
                  bitmap_array, MEMORY_BITMAP_SIZE_8K);

      extensions_add("print", ext_print);

    } else if (strncmp(str, ":prelude", 8) == 0) {

      eval_cps_pause_eval();
      while(eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
        chThdSleepMilliseconds(1);
      }
      prelude_load(&string_tok_state,
                   &string_tok);

      printf("Parsing prelude.\n");
      VALUE prelude = tokpar_parse_program(&string_tok);

      printf("Evaluate prelude.\n");
      eval_cps_program(prelude);

      printf("Eval resuming.\n");
      eval_cps_continue_eval();
    } else if (strncmp(str, ":quit", 5) == 0) {

      break;
    } else if (strncmp(str, ":read", 5) == 0) {
      memset(file_buffer, 0, 2048);
      bool done = false;
      int c;

      for (int i = 0; i < 2048; i ++) {
        c = streamGet(chp);

        if (c == 4 || c == 26 || c == STM_RESET) {
          done = true;
          break;
        }
        file_buffer[i] = (char)c;
      }

      chprintf(chp, "%s\r\n", file_buffer);
      chprintf(chp, "received %d bytes\r\n", strlen(file_buffer));

      if (done) {
        VALUE t;

        /* Get exclusive access to the heap */
        eval_cps_pause_eval();
        while(eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
          sleep_callback(10);
        }
        tokpar_create_char_stream_from_string(&string_tok_state,
                                              &string_tok,
                                              file_buffer);
        t = tokpar_parse(&string_tok);
        CID cid = eval_cps_program(t);
        if (cid == 0) {
          chprintf(chp,"Error creating ctx\r\n");
        } else {
          chprintf(chp,"started ctx: %u\r\n", cid);
        }

        eval_cps_continue_eval();

      }
    } else {

      if (strlen(str) == 0) {
        continue;
      }

      VALUE t;

      /* Get exclusive access to the heap */
      eval_cps_pause_eval();
      while(eval_cps_current_state() != EVAL_CPS_STATE_PAUSED) {
        sleep_callback(10);
      }

      tokpar_create_char_stream_from_string(&string_tok_state,
                                            &string_tok,
                                            str);
      t = tokpar_parse(&string_tok);

      CID cid = eval_cps_program(t);
      if (cid == 0) {
        chprintf(chp,"Error creating ctx\r\n");
      } else {
        chprintf(chp,"started ctx: %u\r\n", cid);
      }

      eval_cps_continue_eval();
    }
  }
}

